#!/usr/bin/env perl
use strict;
use warnings;

use v5.10;
use Data::Dumper;
use Scalar::Util;

use Reader;

my $GLOBAL_ENV = {
		  parent_env => 0,
		  env => {
			  quit => {
				   closure_env => 0,
				   args        => [],
				   lambda_expr => undef,
				   body        => sub { exit; },
				  },
			 },
		 };

REPL: {
    print "* ";
    my $expr;
  INPUT: {
	$expr = scheme_read(0, "\n");
	redo INPUT unless defined($expr);
    }
    print scheme_eval($expr, $GLOBAL_ENV) . "\n";
    redo REPL;
}

sub scheme_eval {
    my ($expr, $env) = @_;
    return scheme_analyze($expr)->($env);
}

sub scheme_analyze {
    my $expr = shift;

    ## REMINDER: NEED TO IMPLEMENT MACRO EXPANSION DURING THIS
    ## PHASE. ADDING SOMETHING TO ANALYZE A MACRO DEFINITION MIGHT BE
    ## NEEDED. ALSO, THERE MUST BE SOME VARIABLE KEPT THAT REMEMBERS
    ## MACRO DEFINITIONS DURING COMPILE TIME SO THEY CAN BE EXPANDED
    ## THERE.

    if (ref $expr eq 'ARRAY') {
	given ($$expr[0]) {
	    when ('string') { return sub { $$expr[1]; }; }
	    when ('quote') { return sub { $$expr[1]; }; }
	    when ('set!') {
		my $var = $$expr[1];
		my $val = scheme_analyze($$expr[2]);
		return sub { 
		    my $env = shift;
		    return $$env{$var} = $val->($env);
		}; }
	    when ('define') {}
	    when ('if') {
		my $pred = scheme_analyze($$expr[1]);
		my $tcl  = scheme_analyze($$expr[2]);
		my $fcl  = defined($$expr[3]) ? scheme_analyze($$expr[3]) : 0;
		return sub {
		    my $env = shift;
		    if ($pred->($env) ne 'nil') {
			return $tcl->($env);
		    }
		    else {
			if ($fcl) {
			    return $fcl->($env);
			}
			else {
			    return 'nil';
			}
		    }
		};
	    }
	    when ('lambda') {
		my @expression = @{ $expr };
		shift @expression; # Cut off the 'LAMBDA'
		my $params = shift @expression;
		my $body = scheme_analyze(['begin', @expression]);

		return sub {
		    my $eval_env = shift;
		    return {
			    closure_env => $eval_env,
			    args        => $params,
			    lambda_expr => \@expression,
			    body        => $body,
			   };
		};
	    }
	    when ('begin') {
		my @block = @{ $expr };
		shift @block;	# Cut off the 'BEGIN'
		my @exprs = map { scheme_analyze($_) } @block;
		my $first_exp = pop @exprs;
		my $seq = sub {
		    my $env = shift;
		    $first_exp->($env);
		};
		while (@exprs) {
		    my $exp = pop @exprs;
		    $seq = sub {
			my $env = shift;
			$exp->($env);
			$seq->($env);
		    };
		}
		return $seq;
	    }
	    default {
		my $func = scheme_analyze(+shift);
		my @arg_list = @_;
		my @arg_procs = map { scheme_analyze($_) } @arg_list;
		my $closure_env = $$func{closure_env};
		return sub {
		    my $outer_env = shift;
		    my @arg_vals = map { $_->($outer_env) } @arg_procs;
		    my %arg_hash  = map { $_ => shift @arg_vals } @arg_list;
		    my $nenv = {
				parent_env => {
					       parent_env => $outer_env,
					       env        => $closure_env,
					      },
				env        => \%arg_hash,
			       };
		};
	    }
	}
    }
    else {
	if (Scalar::Util::looks_like_number($expr)) {
	    return sub {
		return $expr;
	    };
	}
	else {		# Variable
	    return sub {
		my $env = shift;
		return find_var($expr, $env);
	    };
	}
    }
}

sub find_var {
    my ($var, $env) = @_;
    my $this_env = $$env{env};
    if (exists $$this_env{$var}) {
	return $$this_env{$var};
    }
    else {
	if ( $$env{parent_env} ) {
	    my $parent_env = $$env{parent_env};
	    return find_var($var, $parent_env);
	}
	else {
	    return undef;
	}
    }
}
