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
    map { scheme_eval($_, \%GLOBAL_ENV) } @{ $data };
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
#    print STDERR "Done.\nEvaluating...";
    my $evaluated = $analyzed->($env);
#    print STDERR "Done.\n";
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
		my @to_expand = map { ref $_ eq 'ARRAY' ? array_to_cons($_)
					: $_ } @expr;
		my $arg_hash = bind_vars($macro_args, \@to_expand);
		my $expanded = $macro_body->(\%GLOBAL_ENV, $arg_hash);
#		print STDERR "Expansion: @{ [to_string($expanded)] }\n";
		my $to_analyze =
		  ref $expanded eq 'Cons' ? cons_to_array($expanded)
		                          : $expanded;
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
		    if ($pred->($env) ne '#f') {
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
#		    print STDERR "Manual variable lookup: $expr\n";
		    return find_var($expr, $_[0]);
		};
	}
    }
}

sub compile_var_lookup {
    my ($var, $env) = @_;
#    print "Caller: @{ [caller] }\n" unless defined($var);
    my $frames = 0;
    while (defined $env) {
	last unless defined($$env{env});
	last if exists $$env{env}->{$var};
	$frames++;
	$env = $$env{parent_env};
#	die "Undefined var: $var at location: $.\n" unless defined $env;
	return undef unless defined($env); # Dynamic variable lookup
    }
    return sub {
#	print "Looking up: $var Frames: $frames\n";
	my $enviro = shift;
	for (1..$frames) {
	    $enviro = $$enviro{parent_env};
	}
	return $$enviro{env}->{$var};
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
    my $obj = shift;
    my $ret = '';

    given (ref $obj) {
	when ('Cons') {
	    $ret = $obj->to_string(\&to_string);
	}
	when ('String') {
	    $ret = "\"$obj->{string}\"";
	}
	when ('ARRAY') {
	    $ret .= '#(';
	    $ret .= shift @{ $obj };
	    map { $ret .= ' ' . to_string($_) } @{ $obj };
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

## new, broken, but hopfully possessing the potential to be faster version

sub bind_vars {
    my ($syms, $vals) = @_;
    my %new_env = ();

    for my $i (0..(scalar @$syms - 1)) {
	if ($$syms[$i] eq '.') { # slupry
	    $new_env{$$syms[$i+1]} = array_to_cons(\($vals->[$i..-1]));
	    last;
	}
	else {
	    $new_env{$$syms[$i]} = $$vals[$i];
	}
    }
    return \%new_env;
}


## old, slow, but working version

# sub bind_vars {
#     my ($sym_ref, $val_ref) = @_;
#     my @syms = @{ $sym_ref };
#     my @vals = @{ $val_ref };
#     my %env = ();
#     my $slurpy = 0;
#     for my $sym (@syms) {
# 	if ($slurpy) {
# 	    $env{$sym} = array_to_cons(\@vals);
# 	    last;
# 	}
# 	elsif ($sym eq '.') {
# 	    $slurpy = 1;
# 	    next;
# 	}
# 	else {
# 	    $env{$sym} = shift @vals;
# 	}
#     }
#     return \%env;
# }


sub merge_envs {
    my ($parent, $new) = @_;
    return {
	    parent_env => $parent,
	    env        => $new };
}

sub find_var {
    my ($var, $env) = @_;
    my $this_env = $$env{env};
#    print STDERR "Var: @{ [caller] }\n" unless defined($var);
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
		    my @things = @{cons_to_array(find_var('things', $env), 0)};
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
		lambda_expr => undef,
		body        => sub {
		    print "Happy Happy Joy Joy.\n";
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
			error("IN FUNCTION CDR: ARGUMENT IS NOT A LIST.\n");
			error("CDR: CALLER: @{ [caller] }\n");
			return undef;
		    }
		},
	    },
	    rplaca => {
		closure_env => {},
		args        => ['obj', 'new-car'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $cons = find_var('obj', $env);
		    unless (ref $cons eq 'Cons') {
			error("$cons is not a Cons - rplaca\n");
		    }
		    $cons->{car} = find_var('new-car', $env);
		    return $cons;
		},
		      },
	    rplacd => {
		closure_env => {},
		args        => ['obj', 'new-cdr'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $cons = find_var('obj', $env);
		    unless (ref $cons eq 'Cons') {
			error("$cons is not a Cons - rplacd\n");
		    }
		    $cons->{cdr} = find_var('new-cdr', $env);
		    return $cons;
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
		    $arg1 = array_to_cons($arg1) if ref $arg1 eq 'ARRAY';
		    $arg2 = array_to_cons($arg2) if ref $arg2 eq 'ARRAY';
		    return cons($arg1, $arg2);
		},
	    },
	    list => {
		closure_env => {},
		args        => ['.', 'lst'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    return find_var('lst', $env);
		},
	    },
	    'not' => {
		      closure_env => {},
		      args        => ['obj'],
		      lambda_expr => undef,
		      body => sub {
			  my $env = shift;
			  my $obj = find_var('obj', $env);
			  return $obj eq '#f' ? '#t' : '#f';
		      },
		     },
	    'list?' => {
			closure_env => {},
			args        => ['obj'],
			lambda_expr => undef,
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
		lambda_expr => undef,
		body        => sub {
		    my $env = shift;
		    my $func = find_var('function', $env);
		    my $args = find_var('args', $env);

		    $args = cons_to_array($args, 1) if ref $args eq 'Cons';

		    my $nenv = merge_envs($$func{closure_env},
					  bind_vars($$func{args},
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
		    if (ref $cons eq 'Cons') {
			return $cons->null ? '#t' : '#f';
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
		    my @args = @{ cons_to_array(find_var('args', $env), 1) };
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
		    my @args = @{ cons_to_array(find_var('args', $env), 1) };
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
		    my @args = @{ cons_to_array(find_var('args', $env), 1) };
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
		    $args->mapcar(sub { $sum += $_[0]; });
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
		    my $sum = $args->{car};
		    $args = $args->{cdr};
		    if (ref $args eq 'Cons') {
			$args->mapcar(sub { $sum -= shift; });
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
		    $args->mapcar(sub { $prod *= $_[0]; });
		    return $prod;
		},
	    },
	    '/' => {
		closure_env => {},
		args        => ['.', 'args'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $args =find_var('args', $env);
		    my $quot = $args->{car};
		    $args->{cdr}->mapcar(sub { $quot /= $_[0]; });
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
		    my $rem = $args->{car};
		    $args->{cdr}->mapcar(sub { $rem %= $_[0]; });
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
		    return ref $thing eq 'ARRAY' ? array_to_cons($thing)
		                                 : $thing;
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
				 if (open $fh, '<', $file) {
				     print STDERR "Reading $file...";
				     my $data = scheme_read_from_file($fh);
				     print STDERR "Done.\nEvaluating $file...";
				     map { scheme_eval($_, \%GLOBAL_ENV) }
				       @{ $data };
				     print STDERR "Done.\n";
				     return '#t';
				 }
				 else {
				     error("Could not open file: $!\n");
				 }
			     }
			     else {
				 error("File $file not readable\n");
			     }
			 }
			 else {
			     error("File $file does not exist.\n");
			 }
		     },
		    },
	    'write' => {
		args        => ['.', 'strings'],
		lambda_expr => undef,
		closure_env => {},
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    $obj->mapcar(sub { print to_string($_); });
		},
	    },
	    'write-string' => {
		args        => ['.', 'strings'],
		lambda_expr => undef,
		closure_env => {},
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    $obj->mapcar(sub {
				     my $thing = shift;
				     print (ref $thing eq 'String' ? $thing->{string} : to_string($thing)); });
		},
	    },
	    'write-string-err' => {
		args        => ['.', 'strings'],
		lambda_expr => undef,
		closure_env => {},
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    $obj->mapcar(sub {
				     my $thing = shift;
				     print STDERR (ref $thing eq 'String' ? $thing->{string} : to_string($thing)); });
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
		    $obj->mapcar(sub { print STDERR to_string(+shift); });
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
			print to_string(array_to_cons($func->{lambda_expr}));
			return undef;
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
