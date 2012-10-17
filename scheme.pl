#!/usr/bin/env perl
use strict;
use warnings;

use v5.10;
use Data::Dumper;
use Scalar::Util;

use Reader;
use Cons;

my %TRACED_FUNCTIONS = ();
my %MACROS           = ();
my %GLOBAL_ENV       = Special_forms();

REPL: {
    my $init_fh;
    if (open $init_fh, '<', 'init.scm') {
	my $data = scheme_read_from_file($init_fh);
	map { scheme_eval($_, \%GLOBAL_ENV) } @{ $data };
    }

    print "* ";
    my $expr;
  INPUT: {
	$expr = scheme_read(0, "\n");
	redo INPUT unless defined($expr);
    }
    my $to_print = scheme_eval($expr, \%GLOBAL_ENV);
   print "\n" . (defined($to_print) ? to_string($to_print) : "; UNDEF") . "\n";
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
	    when ('quote') { 
		given (ref $$expr[1]) {
		    when ('ARRAY') {
			my $cons = array_to_cons($$expr[1]);
			return sub { $cons; };
		    }
		    default {
			return sub { $$expr[1]; };
		    }
		}
	    }
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
		    return $exprs[$#exprs]->($env); # Deep recursion here
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
	    when (exists $MACROS{$_}) {
		
	    }
	    default {		# Apply

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
	when ('Cons') {
	    $ret = $obj->to_string(\&to_string);
	}
	when ('ARRAY') {
	    $ret .= '#(';
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

sub Special_forms {
    return (
	parent_env => undef,
	env => {
	    '#t' => '#t',
	    '#f' => '#f',
	    nil  => 'nil',
            t    => 't',
	    quit => {
		closure_env => {},
		args        => [],
		lambda_expr => undef,
		body        => sub {
		    print "Happy Happy Joy Joy\n";
		    exit;
		},
	    },
	    car => {
		closure_env => {},
		args        => ['cons-cell'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $cons = find_var('cons-cell', $env);
		    if (ref $cons eq 'ARRAY') {
			return $$cons[0];
		    }
		    elsif (ref $cons eq 'Cons') {
			return $cons->car;
		    }
		    else {
		print STDERR "IN FUNCTION CAR: ARGUMENT IS NOT A LIST.\n";
			return undef;
		    }
		},
	    },
	    cdr => {
		closure_env => {},
		args        => ['cons-cell'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $cons = find_var('cons-cell', $env);
		    if (ref $cons eq 'ARRAY') {
			my @list = @{ $cons };
			my @list2 = @list[1..$#list];
			return \@list2;
		    }
		    elsif (ref $cons eq 'Cons') {
			return $cons->cdr;
		    }
		    else {
		print STDERR "IN FUNCTION CDR: ARGUMENT IS NOT A LIST.\n";
			return undef;
		    }
		},
		
	    },
	    cons => {
		closure_env => {},
		args        => ['arg1', 'arg2'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my ($arg1, $arg2) = 
			map { find_var($_, $env) } qw(arg1 arg2);
		    return cons($arg1, $arg2);
		},
	    },
	    list => {
		closure_env => {},
		args        => ['.', 'lst'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my @things = @{ find_var('lst', $env) };
		    my $end = pop @things;
		    my $cons = cons($end, 'nil');
		    while (@things) {
			$cons = cons(pop @things, $cons);
		    }
		    return $cons;
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

		    $args = cons_to_array($args) if ref $args eq 'Cons';

		    my $nenv = merge_envs($env, bind_vars($$func{args},
							  $args));
		    return $$func{body}->($nenv);
		},
	    },
	    null => {
		closure_env => {},
		args        => ['cons-cell'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $cons = find_var('cons-cell', $env);
		    return $cons->null ? '#t' : '#f';
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
		    my $arg_ref = find_var('args', $env);
		    my @args = @{ $arg_ref };
		    my $sum = shift @args;
		    if (scalar @args) {
			map { $sum -= $_ } @args;
		    }
		    else {
			$sum *= -1;
		    }
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
		    my @args = @{ find_var('args', $env) };
		    my $quot = shift @args;
		    map { $quot /= $_ } @args;
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
		    my $rem = shift @{ $args };
		    map { $rem %= $_ } @{ $args };
		    return $rem;
		},
	    },
	    'write' => {
		args        => ['.', 'strings'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    map { print to_string($_) } @{ $obj };
		    return undef;
		    closure_env => {},
		},
	    },
	    'write-err' => {
		closure_env => {},
		args        => ['.', 'strings'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    map { print STDERR to_string($_) } @{ $obj };
		    return undef;
		},
	    },
	    terpri => {
		closure_env => {},
		args        => [],
		lambda_expr => undef,
		body => sub {
		    print STDERR "\n";
		    return undef;
		},
	    },
	    dumper => {
		closure_env => {},
		args        => ['thing'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $obj = find_var('thing', $env);
		    print STDERR Dumper($obj) . "\n";
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
		    print Dumper( $env );
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
		    return $func;
		},
	    },
	},
	);
}
