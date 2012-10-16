package Cons;
use strict;
use warnings;

use Data::Dumper;

sub import {
    my $caller = caller;
    no strict 'refs';
    *{ $caller . '::' . $_ } = __PACKAGE__->can($_)
      for qw(cons car cdr);
    1;
}

sub cons {
    my $class = "Cons";
    my ($car, $cdr) = @_;
    return bless { car => $car,
		   cdr => $cdr }, $class;
}

sub car {
    my $self = shift;
    return $self->{car};
}

sub cdr {
    my $self = shift;
    return $self->{cdr};
}

sub to_string {
    my ($self, $to_string) = @_;
    my $ret = '(';

    $ret .= ref $self->car eq "CONS" ? $self->to_string()
      : $to_string->($self->car);

    if (ref $self->{cdr} eq 'CONS') {
        $ret .= ' ' . substr($self->cdr->to_string(), 1);
    }
    elsif (ref $self->{cdr} ne 'CONS') {
$ret .= $self->{cdr} eq 'nil' ? ')' : (' ' . $to_string->($self->{cdr}) . ')');
    }
    else {
        $ret .= '. ';
	$ret .= $to_string->($self->{cdr});
        $ret .= ')';
    }

    return $ret;
}

1;
