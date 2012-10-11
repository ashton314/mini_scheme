#!/usr/bin/env perl
use strict;
use warnings;

use Data::Dumper;

use Eval;
use Analyze;
use Reader;

REPL: {
    print "* ";
    my $obj;
  INPUT: {
	$obj = scheme_read(0, "\n");
	redo INPUT unless defined($obj);
    }
    print Dumper($obj) . "\n";
    redo REPL;
}
