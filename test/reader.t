#!/usr/bin/env perl
use strict;
use warnings;

BEGIN {
    push @INC, '..';
}

use Data::Dumper;
use Reader;

while (42) {
    print "Expr: ";
    my $thing = scheme_read(0);
    next unless defined $thing;
    print "Thing: " . Dumper($thing) . "\n\n";
}
