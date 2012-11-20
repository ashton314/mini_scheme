#!/usr/bin/env perl
use strict;
use warnings;
no warnings qw(recursion);

BEGIN { print STDERR "Loading modules....."; }

use v5.10;
use Time::HiRes qw(gettimeofday);
use Data::Dumper;

use Symbol;
use Reader;
use Cons;
use String;

BEGIN { print STDERR "Done.\n"; }

my @FILES_LOADING    = ();
my $FILES_LOADED     = 0;

my %TRACED_FUNCTIONS = ();
my %MACROS           = ();
my %GLOBAL_ENV       = Special_forms();

my $CALLS_TO_ANALYZE = 0;
my $ANALYZE_VERBOSE  = 0;

$| = 1;

my $init_fh;
if (open $init_fh, '<', 'init.scm') {
    print STDERR "Loading init file...";
    my $data = scheme_read_from_file($init_fh);
    print STDERR "Done.\nParsing init file...";
    eval {
	map { scheme_eval($_, \%GLOBAL_ENV) } @{ $data };
    };
    print STDERR "ERROR: $@" if $@;
    print STDERR "Done.\n\n";
}

REPL: {
    print "* ";
    my $expr;
  INPUT: {
	$expr = scheme_read(0, "\n");
	redo INPUT unless defined($expr);
    }
    my $to_print;
    eval {
	$to_print = scheme_eval($expr, \%GLOBAL_ENV);
    };
    print STDERR "$@" if $@;

    print "\n";
    if ($ANALYZE_VERBOSE) {
	print "Calls to analyze: $CALLS_TO_ANALYZE\n";
	$CALLS_TO_ANALYZE = 0;
    }
    print defined($to_print) ? to_string($to_print) : "; UNDEF";
    print "\n";
    redo REPL;
}

sub scheme_eval {
    my ($expr, $env) = @_;
    my $analyzed = scheme_analyze($expr);
    my $evaluated = $analyzed->($env);
    return $evaluated;
}

sub scheme_analyze {
    my $expr = shift;
    my $analyze_env = shift // make_iso_env(\%GLOBAL_ENV);
    $CALLS_TO_ANALYZE++ if $ANALYZE_VERBOSE;

    if (ref $expr eq 'ARRAY') {
	given ($$expr[0]) {
	    when ('string') { return sub { new String($$expr[1]); }; }
	    when ('quote') {
		return sub { return $$expr[1]; };
	    }
	    when ('new-backquote') {
		my @expr = @{ $expr };
		@expr = @expr[1..$#expr];
		my $formatted = backquote_analyze(\@expr);
		my $form = scheme_analyze($formatted, $analyze_env);
		return sub {
		    my $env = shift;
		    return $form->($env);
		};
	    }
	    when ('set!') {
		my $var = $$expr[1];
		my $val = scheme_analyze($$expr[2], $analyze_env);
		return
		  sub {
		    return set_var($var, $_[0], $val->($_[0]));
		}; }
	    when ('define') {
		my @expr = @{ $expr };
		my @expression = @{ $expr };
		shift @expression; # Knock off that 'define'

		if (ref $expr[1] eq 'ARRAY') { # Function def
		    my @arglist    = @{ shift @expression };
		    my @body       = @expression;
		    $$analyze_env{env}->{$expr[1][0]} = 1;
		    my $to_call =
		      scheme_analyze(['set!', (shift @arglist),
				      ['lambda', \@arglist, @body]],
				     $analyze_env);
		    return sub {
			my $env = shift;
			unless (exists $env->{env}{$expr[1][0]}) {
			    $env->{env}{$expr[1][0]} = 1;
			}
			$to_call->($env);
		    };
		}
		else {		# Var def
		    $$analyze_env{env}->{$expr[1]} = 1;
		    my $to_call = scheme_analyze(['set!', (shift @expression),
						  (shift @expression)],
						 $analyze_env);
		    return sub {
			$_[0]->{env}{$expr[1]} = '';
			$to_call->($_[0]);
		    };
		}
	    }
	    when ('defmacro') {
		my @expr = @{ $expr };
		shift @expr; 	# Knock off that 'defmacro'

		my @arg_list = @{ shift @expr };
		my $macro = shift @arg_list;
		my %arg_hash = map { $_ => 1 } @arg_list;
		$$analyze_env{env}->{$macro} = 1;
		$analyze_env = merge_iso_envs(\%arg_hash, $analyze_env);
		my $body = scheme_analyze(['begin', @expr], $analyze_env);

		$MACROS{$macro} = {
		    body => sub {
			my ($env, $args) = @_;
			my $nenv = merge_envs($env, $args);
			return $body->($nenv);
		    },
		    args => \@arg_list,
		};

		return sub { return $macro; };
	    }
	    when (exists $MACROS{$_}) { # Macro expander
		my %macro = %{ $MACROS{$_} };
		my ($macro_body, $macro_args) = map { $macro{$_} }
		  qw(body args);
		my @expr = @{ $expr };
		shift @expr;
		my @to_expand = @expr;
		my $arg_hash = bind_vars($macro_args, \@to_expand);
		my $expanded = $macro_body->(\%GLOBAL_ENV, $arg_hash);
		my $to_analyze = $expanded;
		my $proc = scheme_analyze($to_analyze, $analyze_env);
		return sub {
		    my $env = shift;
		    return $proc->($env);
		};
	    }
	    when ('while') {
		my @expr = @{ $expr };
		shift @expr; 	# Knock off that 'while'

		my $cond = scheme_analyze(shift @expr, $analyze_env);
		my $body = scheme_analyze(['begin', @expr], $analyze_env);
		return sub {
		    my $env = shift;
		    my $res = '#f';
		    while ($cond->($env) ne '#f') {
			$res = $body->($env);
		    }
		    return $res;
		};

	    }
	    when ('if') {
		my $pred = scheme_analyze($$expr[1], $analyze_env);
		my $tcl  = scheme_analyze($$expr[2], $analyze_env);
my $fcl  = defined($$expr[3]) ? scheme_analyze($$expr[3], $analyze_env) : 0;
		return sub {
		    my $env = shift;
		    my $test_val = $pred->($env);
		    if ((! defined($test_val)) || $test_val ne '#f') {
			return $tcl->($env);
		    }
		    else {
			if ($fcl) {
			    return $fcl->($env);
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
		my @exprs = map { scheme_analyze($_, $analyze_env) } @block;
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
		if (ref $params eq 'ARRAY') {
		    my %arg_hash = map { $_ => 1 } @{ $params };
		    $analyze_env = merge_iso_envs(\%arg_hash, $analyze_env);
		}
	my $body = scheme_analyze(['begin', @expression], $analyze_env);

		return sub {
		    my $eval_env = shift;
		    return {
			    closure_env => $eval_env,
			    args        => $params,
			    lambda_expr => \@expression,
			    body        => $body,
			    name        => '#<LAMBDA $body>',
			   };
		};
	    }
	    default {		# Apply

		no warnings;

		my @expression = @{ $expr };
		my ($func_proc, @arg_procs) =
		  map { scheme_analyze($_, $analyze_env) } @expression;

		return sub {
		    my $env = shift;
		    my $func_ref;
		    eval {
			$func_ref = $func_proc->($env);
		    };

		    if (! defined($func_ref) or ref $func_ref ne 'HASH') {
		    	error("Bad function: @{ [$expression[0]]} at $.\n");
		    }

		    # How much here can be done at compile time?
		    # Anything at all?

		    my %func = %{ $func_ref };
		    my @arg_syms = @{ $func{args} }; 
		    my @arg_vals = map { $_->($env) } @arg_procs;
		    my @arg_vals_copy = @arg_vals;
		    my $arg_hash = bind_vars(\@arg_syms, \@arg_vals);
		    my $nenv = merge_envs($func{closure_env},
					  $arg_hash);

		    if ($TRACED_FUNCTIONS{$expression[0]}) {
			print STDERR "CALLING FUNCTION: @{[$expression[0]]}\n";
			print STDERR "            ARGS: @{ [map {to_string($_)} @arg_vals_copy] }\n";
		    }
		    my $result = $func{body}->($nenv);
		    if ($TRACED_FUNCTIONS{$expression[0]}) {
			print STDERR "          RESULT: $result\n";
		    }
		    return $result;
		};
	    }
	}
    }
    else {
	if (looks_like_number($expr)) {
	    return sub {
		return $expr;
	    };
	}
	elsif (substr($expr, 0, 1) eq ':') { # Keywords
	    return sub {
		return $expr;
	    };
	}
	else {		# Variable
	    return 
	      compile_var_lookup($expr, $analyze_env) //
		sub {
		    return find_var($expr, $_[0]);
		};
	}
    }
}

sub compile_var_lookup {
    # Lexical Addressing. This is a _major_ speed boost.
    my ($var, $env) = @_;
    my $frames = 0;
    while (defined $env) {
	last unless defined($$env{env});
	last if exists $$env{env}->{$var};
	$frames++;
	$env = $$env{parent_env};
	return undef unless defined($env); # Dynamic variable lookup
    }
    return sub {
	my $enviro = shift;
	for (1..$frames) {
	    $enviro = $$enviro{parent_env};
	}
	my $val = $$enviro{env}->{$var};
	return $val;
    };
}

sub compile_var_assignment {
    my ($var, $env, $value) = @_;
    my $frames = 0;
    while (defined $env) {
	last unless defined($$env{env});
	last if exists $$env{env}->{$var};
	$frames++;
	$env = $$env{parent_env};
	return undef unless defined($env); # Dynamic variable lookup
    }
    return sub {
	my $enviro = shift;
	my $eval_enviro = $enviro;
	for (1..$frames) {
	    $enviro = $$enviro{parent_env};
	}
	return $$enviro{env}->{$var} = $value->($eval_enviro);
    };
}

sub merge_iso_envs {
    # Takes an env hash, and an enviroment iso, and makes a new
    # enviroment iso with the env hash as `env', and the enviroment
    # iso as `parent_env'.

    my ($env, $iso_parent) = @_;
    return { env => make_iso_hash($env),
	     parent_env => $iso_parent };
}

sub make_iso_env {
    # Takes an enviroment, and returns a new enviroment that is
    # structurally equivalent to the argument, except without the
    # values copied.

    my $env = shift;
    return { env => make_iso_hash($$env{env}),
	     parent_env => (defined($$env{parent_env}) ?
			    make_iso_env($$env{parent_env}) : undef)
	   };
}

sub make_iso_hash {
    # Takes a hash reference, and returns a hash referance that has
    # the same keys (but not values) as the hash ref passed in.

    my $hash_ref = shift;
    my %new_hash = ();
    foreach (keys %{ $hash_ref }) {
	$new_hash{$_} = 1;
    }
    return \%new_hash;
}

sub to_string {
    my $obj = $_[0];
    my $ret = '';

    given (ref $obj) {
	when ('Cons') {
	    $ret = '#' . $obj->to_string(\&to_string);
	}
	when ('String') {
	    $ret = "\"$obj->{string}\"";
	}
	when ('ARRAY') {
	    my @to_print = @{ $obj };
	    $ret .= '(';
	    $ret .= shift @to_print;
	    map { $ret .= ' ' . to_string($_) } @to_print;
	    $ret .= ')';
	}
	when ('HASH') {
	    if (exists $$obj{name}) {
		$ret = $$obj{name};
	    }
	    else {
		$ret = "#<$obj>";
	    }
	}
	default {
	    $ret = "$obj";
	}
    }
    return $ret;
}

sub bind_vars {
    my ($syms, $vals) = @_;
    my %new_env = ();

    for my $i (0..(scalar @$syms - 1)) {
	if ($$syms[$i] eq '.') { # slupry
	    my @rest = @$vals;
	    @rest = @rest[$i..$#rest];
	    $new_env{$$syms[$i+1]} = \@rest;
	    last;
	}
	else {
	    $new_env{$$syms[$i]} = $$vals[$i];
	}
    }
    return \%new_env;
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
	    error("VAR $var NOT DEFINED @{ [caller] }\n");
	    return undef;
	}
    }
}

sub error {
    my $mesg = shift;
    die $mesg;
}

sub backquote_analyze {
    my $thing = shift;
    my $depth = shift // 1;
    return ['append', (map { backquote_loop($_, $depth) } @{ $thing })];
}

sub backquote_loop {
    my $thing = shift;
    my $depth = shift // 1;
    if (ref $thing eq 'ARRAY') {
	given ($$thing[0]) {
	    when ('comma') {
		my $temp = $$thing[1];
		my $i = 0;
		while (ref $temp eq 'ARRAY' and $$temp[0] eq 'comma') {
		    $i++;
		    $temp = $$temp[1];
		    if (ref $temp eq 'ARRAY' and $$temp[0] eq 'comma-splice') {
			$i++;
			if ($i == $depth) {
			    $i = 'splice';
			    last;
			}
			else {
			    error("Comma-splice error near `$thing'\n");
			}
		    }
		}
		if ($i eq 'splice') {
		    return $temp;
		}
		elsif ($i == $depth) {
		    return ['list', $temp];
		}
		else {
		    return ['list', $temp];
		}
	    }
	    when ('comma-splice') {
		return $$thing[1];
	    }
	    when ('new-backquote') {
		my @things = @{ $thing };
		return ['list', [['quote', 'new-backquote'],
				 backquote_analyze($things[1],
						   $depth + 1)]];
	    }
	    default {
		my @things = @{ $thing };
		return backquote_analyze(\@things, $depth);
	    }
	}
    }
    else {
	return ['list', ['quote', $thing]];
    }
}

sub looks_like_number {		# Snarfed from Scalar::Util::PP
  my $num = shift;

  return 0 unless defined($num);

  return 1 if ($num =~ /^[+-]?[0-9]+$/); # is a +/- integer
  return 1
    if ($num =~
	/^([+-]?)(?=[0-9]|\.[0-9])[0-9]*(\.[0-9]*)?([Ee]([+-]?[0-9]+))?$/);
  # a C float
  return 1 if ($] >= 5.008 and $num =~ /^(Inf(inity)?|NaN)$/i) or
    ($] >= 5.006001 and $num =~ /^Inf$/i);
  0;
}

{
    my $counter = 0;
    sub new_symbol {
	$counter++;
	my $name = shift || "GENSYM$counter";
	return {
		name => $name,
		'interned?' => 0,
	       };
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
	    'eq?' => {
		closure_env => {},
		args        => ['.', 'things'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my @things = @{ find_var('things', $env) };
		    error("Got @{ [scalar @things] } args and expected at least 2 -- eq?\n") if scalar @things < 2;
		    my $thing = shift @things;
		    foreach (@things) {
			if (ref $thing ne ref $_ or
			    "$thing" ne "$_") {
			    return '#f';
			}
		    }
		    return '#t';
		},
		     },
	    quit => {
		closure_env => {},
		args        => [],
		lambda_expr => 'quit',
		body        => sub {
		    print "Happy Happy Joy Joy.\n";
		    exit;
		},
	    },
	    car => {
		closure_env => {},
		args        => ['cons-cell'],
		lambda_expr => 'car',
		body => sub {
		    my $env = shift;
		    my $cons = find_var('cons-cell', $env);
		    if (ref $cons eq 'ARRAY') {
			return $$cons[0];
		    }
		    elsif (ref $cons eq 'Cons') {
			print STDERR "Got a Cons -- car\n";
			return $cons->{car};
		    }
		    elsif ($cons eq 'nil') {
			return 'nil';
		    }
		    else {
			error("IN FUNCTION CAR: $cons IS NOT A LIST.\n");
			error("CAR: CALLER: @{ [caller] }\n");
			return undef;
		    }
		},
	    },
	    cdr => {
		closure_env => {},
		args        => ['cons-cell'],
		lambda_expr => 'cdr',
		body => sub {
		    my $env = shift;
		    my $cons = find_var('cons-cell', $env);
		    if (ref $cons eq 'ARRAY') {
			my @list = @{ $cons };
			my @list2 = @list[1..$#list];
			return \@list2;
		    }
		    elsif (ref $cons eq 'Cons') {
			print STDERR "Got a Cons -- car\n";
			return $cons->cdr;
		    }
		    else {
			error("IN FUNCTION CDR: ARGUMENT IS NOT A LIST.\n");
			error("CDR: CALLER: @{ [caller] }\n");
			return undef;
		    }
		},
	    },
	    'last' => {
		closure_env => {},
		args        => ['obj'],
		lambda_expr => 'last',
		body => sub {
		    my $env = shift;
		    my $obj = find_var('obj', $env);
		    return $$obj[-1];
		},
		      },
	    rplaca => {
		closure_env => {},
		args        => ['obj', 'new-car'],
		lambda_expr => 'rplaca',
		body => sub {
		    my $env = shift;
		    my $cons = find_var('obj', $env);
		    $$cons[0] = find_var('new-car', $env);
		    return $cons;
		},
		      },
	    rplacd => {
		closure_env => {},
		args        => ['obj', 'new-cdr'],
		lambda_expr => 'rplacd',
		body => sub {
		    my $env = shift;
		    my $cons = find_var('obj', $env);
		    # FIXME: How on earth do I do this??
		    @$cons[1..-1] = find_var('new-cdr', $env);
		    return $cons;
		},
		      },
	    cons => {
		closure_env => {},
		args        => ['arg1', 'arg2'],
		lambda_expr => 'cons',
		body => sub {
		    my $env = shift;
		    my ($arg1, $arg2) = 
			map { find_var($_, $env) } qw(arg1 arg2);
		    if (ref $arg2 eq 'ARRAY') {
			return [$arg1, @$arg2];
		    }
		    else {
			if ($arg2 eq 'nil') {
			    return [$arg1];
			}
			else {
			    # FIXME: How do I implement dotted notation?
			    return [$arg1, $arg2];
			}
		    }
		},
	    },
	    list => {
		closure_env => {},
		args        => ['.', 'lst'],
		lambda_expr => 'list',
		body => sub {
		    my $env = shift;
		    return find_var('lst', $env);
		},
	    },
	    'not' => {
		      closure_env => {},
		      args        => ['obj'],
		      lambda_expr => 'not',
		      body => sub {
			  my $env = shift;
			  my $obj = find_var('obj', $env);
			  return $obj eq '#f' ? '#t' : '#f';
		      },
		     },
	    'list?' => {
			closure_env => {},
			args        => ['obj'],
			lambda_expr => 'list?',
			body => sub {
			    my $env = shift;
			    my $obj = find_var('obj', $env);
			    if (ref $obj ~~ ['ARRAY', 'Cons']) {
				return '#t';
			    }
			    else {
				return '#f';
			    }
			},
		       },
	    apply => {
		closure_env => {},
		args        => ['function', 'args'],
		lambda_expr => 'apply',
		body        => sub {
		    my $env = shift;
		    my $func = find_var('function', $env);
		    my $args = find_var('args', $env);

		    my $bound = bind_vars($$func{args}, $args);
		    my $nenv = merge_envs($$func{closure_env}, $bound);

		    return $$func{body}->($nenv);
		},
	    },
	    null => {
		closure_env => {},
		args        => ['cons-cell'],
		lambda_expr => 'null',
		body => sub {
		    my $env = shift;
		    my $cons = find_var('cons-cell', $env);
		    if (ref $cons eq 'Cons') {
			return $cons->null ? '#t' : '#f';
		    }
		    elsif (ref $cons eq 'ARRAY') {
			return scalar @$cons == 0 ? '#t' : '#f';
		    }
		    else {
			return $cons eq 'nil' ? '#t' : '#f';
		    }
		},
		    },
	    '>' => {
		closure_env => {},
		args        => ['.', 'args'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my @args = @{ find_var('args', $env) };
		    error("Got @{ [scalar @args] } args and expected at least 2 -- >\n") if scalar @args < 2;
		    my $thing = shift @args;
		    foreach (@args) {
			return '#f' if $_ > $thing;
		    }
		    return '#t';
		},
		   },
	    '<' => {
		closure_env => {},
		args        => ['.', 'args'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my @args = @{ find_var('args', $env) };
		    error("Got @{ [scalar @args] } args and expected at least 2 -- <\n") if scalar @args < 2;
		    my $thing = shift @args;
		    foreach (@args) {
			return '#f' if $_ < $thing;
		    }
		    return '#t';
		},
		   },
	    '=' => {
		closure_env => {},
		args        => ['.', 'args'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my @args = @{ find_var('args', $env) };
		    error("Got @{ [scalar @args] } args and expected at least 2 -- =\n") if scalar @args < 2;
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
		    map { $sum += $_ } @$args;
		    return $sum;
		},
	    },
	    '-' => {
		closure_env => {},
		args        => ['.', 'args'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my @args = @{ find_var('args', $env) };
		    my $sum = shift @args;
		    scalar @args == 0 ? $sum *= -1 :
		      map { $sum -= $_ } @args;
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
		    map { $prod *= $_ } @$args;
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
		    my @args = @{ find_var('args', $env) };
		    my $rem = shift @args;
		    map { $rem %= $_ } @args;
		    return $rem;
		},
	    },
	    'int' => {
		      closure_env => {},
		      args => ['n'],
		      lambda_expr => undef,
		      body => sub {
			  my $env = shift;
			  my $n = find_var('n', $env);
			  return int $n;
		      },
		     },
	    'read' => {
		args => ['stream'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $stream = find_var('stream', $env);
		    my $thing;
		  LOOP: {
			$thing = scheme_read($stream, "\n");
			redo LOOP unless defined($thing);
		    }
		    return $thing;
		},
		      },
            'time' => {
		       args => [],
		       lambda_expr => undef,
		       closure_env => {},
		       body => sub {
			   my ($secs, $mili) = gettimeofday();
			   return "$secs.$mili";
		       },
		      },
	    load => {
		     args => ['file'],
		     lambda_expr => undef,
		     closure_env => {},
		     body => sub {
			 my $env = shift;
			 my $file = (find_var('file', $env))->{string};
			 if (-e $file) {
			     if (-r $file) {
				 my $fh;
				 my $loading = @FILES_LOADING;
				 my $loaded_so_far = $FILES_LOADED;
				 print STDERR "\n" if $loading;
				 print STDERR " " foreach 1..$loading;
				 if (open $fh, '<', $file) {
				     print STDERR "Reading $file...";
				     my $data = scheme_read_from_file($fh);
				     print STDERR "Done.\n";
				     print STDERR (" " x $loading .
						   "Evaluating $file...");
			     push @FILES_LOADING, "Evaluating $file...";
				     map { scheme_eval($_, \%GLOBAL_ENV) }
				       @{ $data };
				     my $self = pop @FILES_LOADING;
				     if ($loaded_so_far != $FILES_LOADED) {
					 print STDERR "\n";
					 print STDERR " " x $loading . $self;
					 print STDERR "Done.\n";
				     }
				     print STDERR "Done.";
				     $FILES_LOADED++;
				     return '#t';
				 }
				 else {
				     error("Could not open file: $!\n");
				 }
			     }
			     else {
				 error("File $file not readable.\n");
			     }
			 }
			 else {
			     error("File $file does not exist.\n");
			 }
		     },
		    },
	    'write' => {
		args        => ['.', 'things'],
		lambda_expr => undef,
		closure_env => {},
		body => sub {
		    my $env = shift;
		    my $objs = find_var('things', $env);
		    map { print to_string($_) } @$objs;
		    return undef;
		},
	    },
	    'write-string' => {
		args        => ['.', 'strings'],
		lambda_expr => undef,
		closure_env => {},
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    map {
			print ref $_ eq 'String' ? $_->{string}
			  : to_string($_) } @$obj;
		    return undef;
		},
	    },
	    'write-string-err' => {
		args        => ['.', 'strings'],
		lambda_expr => undef,
		closure_env => {},
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    map {
			print STDERR (ref $_ eq 'String' ?
				      $_->{string} : to_string($_)) } @$obj;
		    return undef;
		},
	    },
	    'error' => {
		      args => ['error-string'],
		      lambda_expr => undef,
		      closure_env => {},
		      body => sub {
			  my $env = shift;
			  my $str = find_var('error-string', $env) // "ERROR";
			  die $str;
		      },
		     },
	    'write-err' => {
		closure_env => {},
		args        => ['.', 'strings'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    map { print STDERR to_string($_) } @$obj;
		    return undef;
		},
	    },
	    'sleep' => {
			closure_env => {},
			args        => ['num'],
			lambda_expr => undef,
			body => sub {
			    my $env = shift;
			    my $secs = find_var('num', $env);
			    sleep $secs;
			    return undef;
			},
		       },
	    terpri => {
		closure_env => {},
		args        => [],
		lambda_expr => undef,
		body => sub {
		    print "\n";
		    return undef;
		},
	    },
	    'terpri-err' => {
		closure_env => {},
		args        => [],
		lambda_expr => undef,
		body => sub {
		    print STDERR "\n";
		    return undef;
		},
	    },
	    fle => {
		    closure_env => {},
		    args => ['func'],
		    lambda_expr => undef,
		    body => sub {
			my $env = shift;
			my $func = find_var('func', $env);
			print to_string($func->{lambda_expr});
			return undef;
		    },
		   },
	    macroexpand => {
			    closure_env => {},
			    args => ['form'],
			    lambda_expr => undef,
			    body => sub {
				my $env = shift;
				my $form = find_var('form', $env);
				my $form_ref = $form;
if (exists $MACROS{$$form_ref[0]}) { 
    my %macro = %{ $MACROS{$$form_ref[0]} };
    my ($macro_body, $macro_args) = map { $macro{$_} }
      qw(body args);
    my @expr = @{ $form_ref };
    shift @expr;
    my @to_expand = @expr;
    my $arg_hash = bind_vars($macro_args, \@to_expand);
    return $macro_body->(\%GLOBAL_ENV, $arg_hash);
}
			    },
			   },
	    # dumper => {
	    # 	closure_env => {},
	    # 	args        => ['thing'],
	    # 	lambda_expr => undef,
	    # 	body => sub {
	    # 	    my $env = shift;
	    # 	    my $obj = find_var('thing', $env);
	    # 	    print STDERR Dumper($obj) . "\n";
	    # 	    return undef;
	    # 	},
	    # },
	    # closure_env => {
	    # 	closure_env => {},
	    # 	args        => ['symbol'],
	    # 	lambda_expr => undef,
	    # 	body => sub {
	    # 	    my $env = shift;
	    # 	    my $obj = find_var('symbol', $env);
	    # 	    if (ref $obj eq 'HASH') {
	    # 		print Dumper($$obj{closure_env}) . "\n";
	    # 		return undef;
	    # 	    }
	    # 	    else {
	    # 		print "Not a function.\n";
	    # 		return undef;
	    # 	    }
	    # 	},
	    # },
	    gensym => {
		       closure_env => {},
		       args        => [],
		       lambda_expr => undef,
		       body => sub {
			   my $env = shift;
			   my $sym = new_symbol();
			   return $sym;
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
	    # env_dump => {
	    # 	closure_env => {},
	    # 	args        => [],
	    # 	lambda_expr => [],
	    # 	body => sub {
	    # 	    my $env = shift;
	    # 	    print Dumper( $env );
	    # 	    return undef;
	    # 	},
	    # },
	    verbose => {
		closure_env => {},
		args        => ['symbol'],
		lambda_expr => undef,
		body => sub {
		    $ANALYZE_VERBOSE = ! $ANALYZE_VERBOSE;
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
