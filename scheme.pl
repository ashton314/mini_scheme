#!/usr/bin/env perl
use strict;
use warnings;

use v5.10;
use Data::Dumper;
use Scalar::Util;

use Reader;

my %TRACED_FUNCTIONS = ();
my %GLOBAL_ENV = (
    parent_env => undef,
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
	apply => {
	    closure_env => {},
	    args        => ['function', 'args'],
	    lambda_expr => undef,
	    body        => sub {
		my $env = shift;
		my $func = find_var('function', $env);
		my $args = find_var('args', $env);
	    },
	},
	'=' => {
	    closure_env => {},
	    args        => ['.', 'args'],
	    lambda_expr => undef,
	    body => sub {
		my $env = shift;
		my @args = @{ find_var('args', $env) };
		(print "ERROR: Got @{ [scalar @args] } args and expected at least 2 -- =\n" && return undef) if scalar @args < 2;
		my $thing = shift @args;
		foreach (@args) {
		    return '#f' if $_ != $thing;
		}
		return '#t';
	    },
	},
	'+' => {
	    closure_env => {},
	    args        => ['.', 'args'],
	    lambda_expr => undef,
	    body => sub {
		my $env = shift;
		my $args = find_var('args', $env);
		my $sum = 0;
		map { $sum += $_ } @{ $args };
		return $sum;
	    },
	},
	'-' => {
	    closure_env => {},
	    args        => ['.', 'args'],
	    lambda_expr => undef,
	    body => sub {
		my $env = shift;
		my $args = find_var('args', $env);
		my $sum = 0;
		map { $sum -= $_ } @{ $args };
		return $sum;
	    },
	},
	'*' => {
	    closure_env => {},
	    args        => ['.', 'args'],
	    lambda_expr => undef,
	    body => sub {
		my $env = shift;
		my $args = find_var('args', $env);
		my $prod = 1;
		map { $prod *= $_ } @{ $args };
		return $prod;
	    },
	},
	'/' => {
	    closure_env => {},
	    args        => ['.', 'args'],
	    lambda_expr => undef,
	    body => sub {
		my $env = shift;
		my $args = find_var('args', $env);
		my $quot = 1;
		map { $quot /= $_ } @{ $args };
		return $quot;
	    },
	},
	'mod' => {
	    closure_env => {},
	    args        => ['.', 'args'],
	    lambda_expr => undef,
	    body => sub {
		my $env = shift;
		my $args = find_var('args', $env);
		my $rem = 0;
		map { $rem %= $_ } @{ $args };
		return $rem;
	    },
	},
	'write' => {
		    closure_env => {},
		    args        => ['string'],
		    lambda_expr => undef,
		    body => sub {
			my $env = shift;
			my $obj = find_var('string', $env);
			print to_string($obj);
			return undef;
		    },
		   },
	closure_env => {
	    closure_env => {},
	    args        => ['symbol'],
	    lambda_expr => undef,
	    body => sub {
		my $env = shift;
		my $obj = find_var('symbol', $env);
		if (ref $obj eq 'HASH') {
		    print Dumper($$obj{closure_env}) . "\n";
		    return undef;
		}
		else {
		    print "Not a function.\n";
		    return undef;
		}
	    },
	},
	env_symbols => {
			closure_env => {},
			args        => [],
			lambda_expr => [],
			body => sub {
			    my $env = shift;
			    my @syms = keys %{ $$env{env} };
			    return \@syms;
			},
		       },
	env_dump => {
		closure_env => {},
		args        => [],
		lambda_expr => [],
		body => sub {
		    my $env = shift;
		    print Dumper( $$env{env} );
		    return undef;
		},
	       },
	trace => {
	    closure_env => {},
	    args        => ['symbol'],
	    lambda_expr => undef,
	    body => sub {
		my $env = shift;
		my $func = find_var('symbol', $env);
		$TRACED_FUNCTIONS{$func} = (exists $TRACED_FUNCTIONS{$func} &&
					    $TRACED_FUNCTIONS{$func}) ? 0 : 1;
		print $TRACED_FUNCTIONS{$func} ? "TRACED\n" : "UNTRACED\n";
		return undef;
	    },
	},
    },
    );


REPL: {
    print "* ";
    my $expr;
  INPUT: {
	$expr = scheme_read(0, "\n");
	redo INPUT unless defined($expr);
    }
    my $to_print = scheme_eval($expr, \%GLOBAL_ENV);
    print "\n" . (defined($to_print) ? to_string($to_print) : ";UNDEF") . "\n";
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
		    return set_var($var, $_[0], $val->($_[0]));
		}; }
	    when ('define') {
		my @expr = @{ $expr };
		my @expression = @{ $expr };
		shift @expression; # Knock off that 'define'

		if (ref $expr[1] eq 'ARRAY') { # Function def
		    my @arglist    = @{ shift @expression };
		    my @body       = @expression;
		    my $to_call =
		      scheme_analyze(['set!', (shift @arglist),
				      ['lambda', \@arglist, @body]]);
		    return sub {
			my $env = shift;
			unless (exists $env->{env}{$expr[1][0]}) {
			    $env->{env}{$expr[1][0]} = 1;
			}
			$to_call->($env);
		    };
		}
		else {		# Var def
		    my $to_call = scheme_analyze(['set!', (shift @expression),
						  (shift @expression)]);
		    return sub {
			$_[0]->{env}{$expr[1]} = '';
			$to_call->($_[0]);
		    };
		}
	    }
	    when ('if') {
		my $pred = scheme_analyze($$expr[1]);
		my $tcl  = scheme_analyze($$expr[2]);
		my $fcl  = defined($$expr[3]) ? scheme_analyze($$expr[3]) : 0;
		return sub {
		    if ($pred->($_[0]) ne '#f') {
			return $tcl->($_[0]);
		    }
		    else {
			if ($fcl) {
			    return $fcl->($_[0]);
			}
			else {
			    return '#f';
			}
		    }
		};
	    }
	    when ('begin') {
		my $env = shift;
		my @block = @{ $expr };
		shift @block;	# Cut off the 'BEGIN'
		my @exprs = map { scheme_analyze($_) } @block;
		return sub {
		    my $env = shift;
		    for my $expr (@exprs[0..($#exprs-1)]) {
			$expr->($env);
		    }
		    return $exprs[$#exprs]->($env);
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
		    my @arg_vals_copy = @arg_vals;
		    my $arg_hash = bind_vars(\@arg_syms, \@arg_vals);
		    my $nenv = merge_envs($func{closure_env},
					  $arg_hash);

		    if ($TRACED_FUNCTIONS{$expression[0]}) {
			print "CALLING FUNCTION: @{ [$expression[0]] }\n";
			print "            ARGS: @arg_vals_copy\n";
		    }
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
		return find_var($expr, $_[0]);
	    };
	}
    }
}

sub to_string {
    my $obj = shift;
    my $ret = '';

    given (ref $obj) {
	when ('ARRAY') {
	    $ret .= '(';
	    $ret .= shift @{ $obj };
	    map { $ret .= ' ' . to_string($_) } @{ $obj };
	    $ret .= ')';
	}
	default {
	    $ret = "$obj";
	}
    }
    return $ret;
}

sub bind_vars {
    my ($sym_ref, $val_ref) = @_;
    my @syms = @{ $sym_ref };
    my @vals = @{ $val_ref };
    my %env = ();
    my $slurpy = 0;
    for my $sym (@syms) {
	if ($slurpy) {
	    $env{$sym} = \@vals;
	    last;
	}
	elsif ($sym eq '.') {
	    $slurpy = 1;
	    next;
	}
	else {
	    $env{$sym} = shift @vals;
	}
    }
    return \%env;
}

sub merge_envs {
    my ($parent, $new) = @_;
    return {
	    parent_env => $parent,
	    env        => $new };
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

sub set_var {			# setf
    my ($var, $env, $val) = @_;
    my $this_env = $$env{env};
    if (exists $$this_env{$var}) {
	$$env{env}->{$var} = $val;
	return $val;
    }
    else {
	if ($$env{parent_env}) {
	    return set_var($var, $$env{parent_env}, $val);
	}
	else {
	    print STDERR "ERROR: VAR $var NOT DEFINED\n";
	    return undef;
	}
    }
}
