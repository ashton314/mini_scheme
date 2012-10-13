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
				   closure_env => {},
				   args        => [],
				   lambda_expr => undef,
				   body        => sub {
				       print "Happy Happy Joy Joy\n";
				       exit;
				   },
				  },
			  '+-primitive' => {
					    closure_env => {},
					    args        => ['one', 'two'],
					    lambda_expr => undef,
					    body => sub {
						my $env = shift;
						my $one = find_var('one', $env);
						my $two = find_var('two', $env);
						return $one + $two;
					    },
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
    print "\n" . scheme_eval($expr, $GLOBAL_ENV) . "\n";
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
		    my $inner_env = $$env{env};
		    return ($$inner_env{$var} = $val->($env));
		}; }
	    when ('define') {
		my @expr = @{ $expr };
		my @expression = @{ $expr };
		shift @expression; # Knock off that 'define'

		if (ref $expr[1] eq 'ARRAY') { # Function def
		    my @arglist    = @{ shift @expression };
		    my @body       = @expression;
		    my $to_analyze = ['set!', (shift @arglist),
					   ['lambda', \@arglist, @body]];
		    return scheme_analyze($to_analyze);
		}
		else {		# Var def
		    return scheme_analyze(['set!', (shift @expression),
					   (shift @expression)]);
		}
	    }
	    when ('if') {
		my $pred = scheme_analyze($$expr[1]);
		my $tcl  = scheme_analyze($$expr[2]);
		my $fcl  = defined($$expr[3]) ? scheme_analyze($$expr[3]) : 0;
		return sub {
		    my $env = shift;
		    if ($pred->($env) ne '#f') {
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
	    default {

		no warnings;

		my @expression = @{ $expr };
		my ($func_proc, @arg_procs) =
		  map { scheme_analyze($_) } @expression;

		return sub {
		    my $env = shift;
		    my %func = %{ $func_proc->($env) };
		    my @arg_syms = @{ $func{args} };
		    my @arg_vals = map { $_->($env) } @arg_procs;
		    my %arg_hash = map { (shift @arg_syms) => $_ } @arg_vals;
		    my $nenv = {
				parent_env => {
					       parent_env => $env,
					       env       => $func{closure_env},
					      },
				env        => \%arg_hash,
			       };
		    return $func{body}->($nenv);
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
#		print Dumper($env) . "\n";
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
