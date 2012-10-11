#!/usr/bin/env perl
use strict;
use warnings;

BEGIN {
    push @INC, '..';
}

use Data::Dumper;
use Reader;

print "File: ";
chomp(my $file = <STDIN>);

open my $fh, '<', $file or die "open: $!";

my $data = scheme_read_from_file($fh);
print "From file:\n" . Dumper($data) . "\n";
