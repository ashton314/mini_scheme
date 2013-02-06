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

