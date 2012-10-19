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

    if (ref $self eq 'Cons') {
	return $self;
    }
    elsif (ref $self ne 'ARRAY') {
	print STDERR "Argument is not an array ref. -- array_to_cons\n";
	return undef;
    }
    else {
	my @things = @{ $self };
	my $end = pop @things;
	return 'nil' unless $end;
	my $cons = cons((ref $end eq 'ARRAY' ? array_to_cons($end) : $end),
			'nil');
	while (@things) {
	    my $obj = pop @things;
	    if ($obj eq '.') {
		my $next = pop @things;
		$cons = cons((ref $next eq 'ARRAY' ? array_to_cons($next)
			                           : $next),
			     $cons->{car});
	    }
	    else {
		$cons = cons((ref $obj eq 'ARRAY' ? array_to_cons($obj)
			                          : $obj),
			     $cons);
	    }
	}
	return $cons;
    }
}

sub cons_to_array {
    my $self = shift;
    my $depth = shift;

    print "Argument is not a Cons. -- cons_to_array\n" && return undef
      unless ref $self eq 'Cons';

    my @list = ();
    my $this = $self;
  LOOP: {
	my $car = $this->{car};
	if (ref $car eq 'Cons') {
	    push @list, $depth ? $car->cons_to_array($depth - 1) : $car;
	}
	else {
	    push @list, $car;
	}
	if (ref $this->{cdr} eq 'Cons') {
	    $this = $this->{cdr};
	    redo LOOP;
	}
	else {			# End of list.
	    push @list, $this->{cdr} unless $this->{cdr} eq 'nil';
	    last LOOP;
	}
    }
    return \@list;
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
