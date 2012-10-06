#!/usr/bin/env perl
use strict;
use warnings;

use Data::Dumper;

require '../Reader.pm';

print "Expr: ";
my $thing = Reader::scheme_read(0, "\n");
print "Thing: " . Dumper($thing) . "\n";
