#!/usr/bin/env perl
use strict;
use warnings;

use Data::Dumper;

require '../Reader.pm';

open my $fh, '<', 'data_file1.lisp';
my $good = 1;
while ($good) {
    my $data = Reader::scheme_read(Reader::make_scheme_stream($fh));
    print "From file:\n" . Dumper($data) . "\n";
    $good = $data;
}
