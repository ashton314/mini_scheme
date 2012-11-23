#!/usr/bin/env perl
use strict;
use warnings;

use Data::Dumper;

my $lst = [1, 2, 3, 4, 5];
my $cdr1 = cdr($lst);
my $cdr2 = cdr($lst);

print "Cdr1: $cdr1\n" . Dumper($cdr1);
print "Cdr2: $cdr2\n" . Dumper($cdr2);

if ($cdr1 == $cdr2) {
    print "Success!\n";
}
else {
    print "Fail.\n";
}

sub cdr {
    my $refr = shift;
    my @foo = @{ $refr };
    @foo = @foo[1..$#foo];
    return \@foo;
}
