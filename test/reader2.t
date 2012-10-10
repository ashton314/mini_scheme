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
open my $fh, '<', $file;
my $good = 1;
while ($good) {
    my $data = scheme_read(make_scheme_stream($fh));
    print "From file:\n" . Dumper($data) . "\n";
    $good = $data;
}
