#!/usr/bin/env perl
use strict;
use warnings;

BEGIN {
    push @INC, '..';
}

use Data::Dumper;
use Reader;

print "Expr: ";
my $thing = scheme_read(0);
print "Thing: " . Dumper($thing) . "\n";
