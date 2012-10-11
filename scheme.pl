#!/usr/bin/env perl
use strict;
use warnings;

use Data::Dumper;

use Eval;
use Analyze;
use Reader;

REPL: {
    print "* ";
    my $obj = scheme_read(0);
    print Dumper($obj) . "\n";
    redo REPL;
}
