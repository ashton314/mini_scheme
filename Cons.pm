package Cons;
use strict;
use warnings;

use Data::Dumper;

sub import {
    my $caller = caller;
    no strict 'refs';
    *{ $caller . '::' . $_ } = __PACKAGE__->can($_)
      for qw(cons car cdr null array_to_cons cons_to_array);
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

sub null {
    my $self = shift;
    return scalar @{ cons_to_array($self) } ? 0 : 1;
}

sub array_to_cons {
    my $self = shift;
    if (ref $self ne 'ARRAY') {
	print STDERR "Argument is not an array ref. -- array_to_cons\n";
	return undef;
    }
    else {
	my @things = @{ $self };
	my $end = pop @things;
	my $cons = cons($end, 'nil');
	$cons = cons(pop @things, $cons) while @things;
	return $cons;
    }
}

sub cons_to_array {
    my $self = shift;
    print "Argument is not a Cons. -- cons_to_array\n" && return undef
      unless ref $self eq 'Cons';

    my $car = $self->{car};
    if (ref $car eq 'Cons') {
	return [cons_to_array($car), cons_to_array($self->{cdr})];
    }
    elsif ($self->{cdr} eq 'nil' or ref $self->{cdr} ne 'Cons') {
	return $self->{cdr};
    }
    else {
	return [$car, cons_to_array($self->{cdr})];
    }
}

sub to_string {
    my ($self, $to_string) = @_;
    my $ret = '(';

    $ret .= ref $self->car eq 'Cons' ? $self->{car}->to_string($to_string)
      : $to_string->($self->car);

    if (ref $self->{cdr} eq 'Cons') {
        $ret .= ' ' . substr($self->cdr->to_string($to_string), 1);
    }
    elsif (ref $self->{cdr} ne 'Cons') {
	$ret .= $self->{cdr} eq 'nil' ? ')' :
	        (' . ' . $to_string->($self->{cdr}) . ')');
    }
    else {
        $ret .= '. ';
	$ret .= $to_string->($self->{cdr});
        $ret .= ')';
    }

    return $ret;
}

1;
