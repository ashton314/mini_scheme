#!/usr/bin/env perl
use strict;
use warnings;
no warnings qw(recursion);

BEGIN {
    push @INC, 'mods/';
    print STDERR "Loading modules.....";
}

## General modules
use v5.10;
use Getopt::Long;
use Data::Dumper;
use Time::HiRes qw(gettimeofday);

## Custom modules for Scheme
use Reader;
use Cons;
use String;

BEGIN { print STDERR "Done.\n"; }

## Options
my ($NO_INIT, $QUIET, $NO_STATS) = (0, 0, 0);
GetOptions('no-init'  => \$NO_INIT,   # Disable init file load
	   quiet      => \$QUIET,     # Suppress load messages
	   'no-stats' => \$NO_STATS); # Suppress memory statistics

## Global vars
my @FILES_LOADING    = ();
my $FILES_LOADED     = 0;

my %TRACED_FUNCTIONS = ();
my %MACROS           = ();
my %GLOBAL_ENV       = Special_forms();

my $CALLS_TO_ANALYZE = 0;
my $ANALYZE_VERBOSE  = 0;

## Init file
if ($NO_INIT) {
    print STDERR "Skipping init file load.\n\n" unless $QUIET;
}
else {
    my $init_fh;
    if (open $init_fh, '<', 'init.scm') {
	print STDERR "Loading init file..." unless $QUIET;
	my $data = scheme_read_from_file($init_fh);
	print STDERR "Done.\nParsing init file..." unless $QUIET;
	eval {
	    map { scheme_eval($_, \%GLOBAL_ENV) } @{ $data };
	};
	print STDERR "ERROR: $@" if $@;
	print STDERR "Done.\n\n" unless $QUIET;
    }
}

## Memory statistics
unless ($NO_STATS) {
    print "Memory statistics:\n";
    print "USER     PID \%CPU \%MEM   VSZ   RSS  TT  STAT STARTED      TIME COMMAND\n";
    print `ps u | grep perl | grep -v grep`;
    print "\n";
}

END {
    unless ($NO_STATS) {
	print "Memory statistics:\nUSER     PID \%CPU \%MEM   VSZ   RSS  TT  STAT STARTED      TIME COMMAND\n";
	print `ps u | grep perl | grep -v grep`;
	print "\n";
    }
}


REPL: {
    print "* ";
    my $expr;
    $expr = scheme_read(0, "\n") until defined $expr;
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
		my $val = scheme_analyze($$expr[2], $analyze_env);
		return
		  sub {
		      return set_var($var, $_[0], $val->($_[0]));
		  };
	    }
	    when ('define') {
		my @expr = @{ $expr };
		my @expression = @{ $expr };
		shift @expression; # Knock off that 'define'

		if (ref $expr[1] eq 'ARRAY') { # Function def
		    my @arglist = @{ shift @expression };
		    my @body    = @expression;
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
		    my $to_call;

		    unless ($expression[1]) {
			$to_call = scheme_analyze(['set!', (shift @expression),
						   'undef'], $analyze_env);
		    }
		    else {
			$to_call = scheme_analyze(['set!', (shift @expression),
						   (shift @expression)],
						  $analyze_env);
		    }

		    return sub {
			$_[0]->{env}{$expr[1]} = '';
			$to_call->($_[0]);
		    };
		}
	    }
	    when ('define-macro') {
		my @expr = @{ $expr };
		shift @expr; 	# Knock off that 'define-macro'

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

		    return $exprs[$#exprs] ? $exprs[$#exprs]->($env) : undef;

		    # Attempt at tail-call optimizations:
		    # @_ = ($env);
		    # goto &{$exprs[$#exprs]};
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
			    name        => '__ANNON__',
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
	return $$enviro{env}->{$var};
    };
}

## When I tested this, this seemed to have a negative effect on
## execution speed.
# sub compile_var_assignment {
#     my ($var, $env, $value) = @_;
#     my $frames = 0;
#     while (defined $env) {
# 	last unless defined($$env{env});
# 	last if exists $$env{env}->{$var};
# 	$frames++;
# 	$env = $$env{parent_env};
# 	return undef unless defined($env); # Dynamic variable lookup
#     }
#     return sub {
# 	my $enviro = shift;
# 	my $eval_enviro = $enviro;
# 	for (1..$frames) {
# 	    $enviro = $$enviro{parent_env};
# 	}
# 	return $$enviro{env}->{$var} = $value->($eval_enviro);
#     };
# }

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

sub bind_vars {
    my ($syms, $vals) = @_;
    my %new_env = ();

    my $sym_length = scalar(@$syms) // 0;
    my $val_length = scalar(@$vals) // 0;

    my $max = 0;
    for my $i (0..($sym_length - 1)) {
	if ($$syms[$i] eq '.' or $$syms[$i] eq '&rest') { # slupry
	    my @rest = @$vals;
	    @rest = @rest[$i..$#rest];
	    $new_env{$$syms[$i+1]} = array_to_cons(\@rest);
	    $max = -1;
	    last;
	}
	elsif ($val_length <= $i) {
	    error("Too few args: got $val_length, expected at $sym_length.\n");
	}
	else {
	    $new_env{$$syms[$i]} = $$vals[$i];
	}
	$max++;
    }
    if ($max != -1 && $val_length > $max &&
	($sym_length ? $$syms[$max] ne '.' || $$syms[$max] ne '&rest' : 1)) {
    	error("Too many args: got $val_length, expected $sym_length.\n");
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
    my ($var, $env, $func) = @_;
    my $this_env = $$env{env};
    if (exists $$this_env{$var}) {
	if (defined($func)) {
	    return $func->($$this_env{$var});
	}
	else {
	    return $$this_env{$var};
	}
    }
    else {
	if ( $$env{parent_env} ) {
	    my $parent_env = $$env{parent_env};
	    return find_var($var, $parent_env, $func);
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
	    error("var $var not defined @{ [caller] }\n");
	    return undef;
	}
    }
}

sub error {
    my $mesg = shift;
    die $mesg;
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
	    'undef' => undef,
	    'eq?' => {
		closure_env => {},
		args        => ['.', 'args'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $args = find_var('args', $env);
		    my $thing = $args->{car};
		    my $eq = 1;
		    $args->{cdr}->mapcar(sub {
					     my $i = shift;
					     if (ref $i ne ref $thing or
						 "$i" ne "$thing") {
						 $eq = 0;
					     } });
		    return $eq ? '#t' : '#f';
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
			return $cons->{car};
		    }
		    elsif ($cons eq 'nil') {
			return 'nil';
		    }
		    else {
			error("IN FUNCTION CAR: @{ [to_string($cons)] } IS NOT A LIST.\nCAR: CALLER: @{ [caller] }\n");
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
			return $cons->cdr;
		    }
		    elsif ($cons eq 'nil') {
			return 'nil';
		    }
		    else {
			error("IN FUNCTION CDR: @{ [to_string($cons)] } IS NOT A LIST.\nCDR: CALLER: @{ [caller] }\n");
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
		    error("$obj is not a Cons - last\n")
			unless ref $obj eq 'Cons';
		    return $obj->{last};
		},
	    },
	    rplaca => {
		closure_env => {},
		args        => ['obj', 'new-car'],
		lambda_expr => 'rplaca',
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
		lambda_expr => 'rplacd',
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
		lambda_expr => 'cons',
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
	    'number?' => {
		closure_env => {},
		args => ['num'],
		lambda_expr => 'number?',
		body => sub {
		    my $env = shift;
		    my $num = find_var('num', $env);
		    return looks_like_number($num) ? '#t' : '#f';
		},
	     },
	    'macro?' => {
		closure_env => {},
		args        => ['sym'],
		lambda_expr => 'macro?',
		body => sub {
		    my $env = shift;
		    my $sym = find_var('sym', $env);
		    return exists $MACROS{$sym} ? '#t' : '#f';
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

		    $args = cons_to_array($args, 1) if ref $args eq 'Cons';

		    my $bound = bind_vars($$func{args}, $args);
		    my $nenv = merge_envs($$func{closure_env}, $bound);

		    return $$func{body}->($nenv);
		},
	    },
	    'null?' => {
		closure_env => {},
		args        => ['cons-cell'],
		lambda_expr => 'null?',
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
		    error("@{ [to_string($thing)] } is not numeric in comparison! -- =")
			if ref $thing;
		    foreach (@args) {
			error("@{ [to_string($_)] } is not numeric in comparison! -- =")
			    if ref $_;
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
	    'clear' => {
		args => [],
		lambda_expr => 'clear',
		closure_env => {},
		body => sub {
		    system('clear');
		    return undef;
		},
	    },
            'time' => {
		args => [],
		lambda_expr => undef,
		closure_env => {},
		body => sub {
		    my ($secs, $mili) = gettimeofday();
		    return sprintf("%d.%06d", $secs, $mili);
		},
	    },
	    load => {
		args => ['file'],
		lambda_expr => undef,
		closure_env => \%GLOBAL_ENV,
		body => sub {
		    my $env = shift;
		    my $file = (find_var('file', $env))->{string};
		    if (-e $file) {
			if (-r $file) {
			    # Check for compiled files
			    my $info;
			    eval {
			    	$info = Storable::file_magic($file);
			    };
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
		    $objs->mapcar(sub { print to_string(+shift); });
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
	    macroexpand => {
		closure_env => {},
		args => ['form'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $form = find_var('form', $env);
		    my $form_ref = cons_to_array($form);
		    if (exists $MACROS{$$form_ref[0]}) { 
			my %macro = %{ $MACROS{$$form_ref[0]} };
			my ($macro_body, $macro_args) = map { $macro{$_} }
			qw(body args);
			my @expr = @{ $form_ref };
			shift @expr;
			my @to_expand = map { ref $_ eq 'ARRAY' ? array_to_cons($_)
						  : $_ } @expr;
			my $arg_hash = bind_vars($macro_args, \@to_expand);
			return $macro_body->(\%GLOBAL_ENV, $arg_hash);
		    }
		},
	    },
	    implode => {
		closure_env => {},
		args => ['cons-cell'],
		lambda_expr => 'implode',
		body => sub {
		    my $env = shift;
		    my $cons = find_var('cons-cell', $env);
		    my $sym = '';
		    $cons->mapcar(sub { $sym .= +shift; });
		    return $sym;
		},
	     },
	     explode => {
		closure_env => {},
		args => ['sym'],
		lambda_expr => 'explode',
		body => sub {
		    my $env = shift;
		    my $sym = find_var('sym', $env);
		    return array_to_cons([split //, $sym]);
		},
	     },
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
	    verbose => {
		closure_env => {},
		args        => ['symbol'],
		lambda_expr => undef,
		body => sub {
		    $ANALYZE_VERBOSE = ! $ANALYZE_VERBOSE;
		},
	    },
	    memory => {
		closure_env => {},
		args        => [],
	        lambda_expr => 'memory',
		body => sub {
		    print "Memory statistics:\n";
		    print "USER     PID \%CPU \%MEM   VSZ   RSS  TT  STAT STARTED      TIME COMMAND\n";
		    print `ps u | grep perl | grep -v grep`;
		    print "\n";
		},
	    },
	    dumper => {
		closure_env => {},
		args        => ['thing'],
		lambda_expr => 'dumper',
		body => sub {
		    my $env = shift;
		    my $thing = find_var('thing', $env);
		    print Dumper($thing);
		    return undef;
		},
            },
	    dump_env => {
		closure_env => \%GLOBAL_ENV,
		args        => ['.', 'syms'],
		lambda_expr => 'dump_env',
		body => sub {
		    my $env = shift;
		    my $syms = find_var('syms', $env);
		    if ($syms eq 'nil' || $syms->null()) {
			print Dumper($env);
		    }
		    else {
			$syms->mapcar(sub {
					  find_var(+shift, $env,
						   sub {
						       print Dumper(+shift);
						   });
				      });
		    }
		    return undef;
		},
	    },
	    trace => {
		closure_env => {},
		args        => ['symbol'],
		lambda_expr => 'trace',
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
