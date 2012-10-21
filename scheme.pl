#!/usr/bin/env perl
use strict;
use warnings;

BEGIN { print STDERR "Loading modules....."; }

use v5.10;

use Reader;
use Cons;

BEGIN { print STDERR "Done.\n"; }

my %TRACED_FUNCTIONS = ();
my %MACROS           = ();
my %GLOBAL_ENV       = Special_forms();

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
    my $to_print = scheme_eval($expr, \%GLOBAL_ENV);
   print "\n" . (defined($to_print) ? to_string($to_print) : "; UNDEF") . "\n";
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
#    print STDERR "Analyzing....";

    my $expr = shift;

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
	    when ('defmacro') {
		my @expr = @{ $expr };
		shift @expr; 	# Knock off that 'defmacro'

		my @arg_list = @{ shift @expr };
		my $macro = shift @arg_list;
		my $body = scheme_analyze(['begin', @expr]);

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
		my $to_analyze = cons_to_array($expanded);
		my $proc = scheme_analyze($to_analyze);
		return sub {
		    my $env = shift;
		    return $proc->($env);
		};
	    }
	    when ('while') {
		my @expr = @{ $expr };
		shift @expr; 	# Knock off that 'while'

		my $cond = scheme_analyze(shift @expr);
		my $body = scheme_analyze(['begin', @expr]);
		return sub {
		    my $env = shift;
		    my $res = '#f';
		    while ($cond->($env) ne '#f') {
			$res = $body->($env);
		    }
		    return $res;
		};

	    }
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
	    default {		# Apply

		no warnings;

		my @expression = @{ $expr };
		my ($func_proc, @arg_procs) =
		  map { scheme_analyze($_) } @expression;

		return sub {
		    my $env = shift;
		    my $func_ref;
		    eval {
			$func_ref = $func_proc->($env);
		    };
		    if (! defined($func_ref) or ref $func_ref ne 'HASH') {
		    	error("Bad function: @{ [$expression[0]]}\n");
		    }
		    my %func = %{ $func_ref };
		    my @arg_syms = @{ $func{args} };
		    my @arg_vals = map { $_->($env) } @arg_procs;
		    my @arg_vals_copy = @arg_vals;
		    my $arg_hash = bind_vars(\@arg_syms, \@arg_vals);
		    my $nenv = merge_envs($func{closure_env},
					  $arg_hash);

		    if ($TRACED_FUNCTIONS{$expression[0]}) {
			print "CALLING FUNCTION: @{ [$expression[0]] }\n";
			print "            ARGS: @{ [map {to_string($_)} @arg_vals_copy] }\n";
		    }
		    return $func{body}->($nenv);
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
#    print STDERR "Caller: @{ [caller] }\n";
    my @syms = @{ $sym_ref };
    my @vals = @{ $val_ref };
    my %env = ();
    my $slurpy = 0;
    for my $sym (@syms) {
	if ($slurpy) {
	    $env{$sym} = array_to_cons(\@vals);
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
    print STDERR "Var: @{ [caller] }\n" unless defined($var);
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
	    error("VAR $var NOT DEFINED\n");
	    return undef;
	}
    }
}

sub error {
    my $mesg = shift;
    print STDERR $mesg;
    return undef;
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
		    my @things = @{ cons_to_array(find_var('things', $env), 0) };
#		    print "Things: @things\n";
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

		    $args = cons_to_array($args) if ref $args eq 'Cons';

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
		    my @args = @{ cons_to_array(find_var('args', $env)) };
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
		    my @args = @{ cons_to_array(find_var('args', $env)) };
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
		    my @args = @{ cons_to_array(find_var('args', $env)) };
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
		    map { $sum += $_ } @{ cons_to_array($args) };
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
		    my @args = @{ cons_to_array($arg_ref) };
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
		    map { $prod *= $_ } @{ cons_to_array($args) };
		    return $prod;
		},
	    },
	    '/' => {
		closure_env => {},
		args        => ['.', 'args'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my @args = @{ cons_to_array(find_var('args', $env)) };
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
		    my @vals = @{ cons_to_array($args) };
		    my $rem = shift @vals;
		    map { $rem %= $_ } @vals;
		    return $rem;
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
		    return ref $thing eq 'ARRAY' ? array_to_cons($thing) : $thing;
		},
		      },
	    load => {
		     args => ['file'],
		     lambda_expr => undef,
		     closure_env => {},
		     body => sub {
			 my $env = shift;
			 my $file = find_var('file', $env);
			 if (-e $file) {
			     if (-r $file) {
				 my $fh;
				 if (open $fh, '<', $file) {
				     my $data = scheme_read_from_file($fh);
				     map { scheme_eval($_, \%GLOBAL_ENV) }
				       @{ $data };
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
		    map { print to_string($_) } @{ cons_to_array($obj, 0) };
		    return undef;
		},
	    },
	    'write-err' => {
		closure_env => {},
		args        => ['.', 'strings'],
		lambda_expr => undef,
		body => sub {
		    my $env = shift;
		    my $obj = find_var('strings', $env);
		    map { print STDERR to_string($_) } @{ cons_to_array($obj, 0) };
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
