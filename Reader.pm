package Reader;
use strict;
use warnings;

use Data::Dumper;

sub import {
    my $caller = caller;
    no strict 'refs';
    *{ $caller . '::' . $_ } = __PACKAGE__->can( $_ )
      for qw(scheme_read scheme_read_delimited_list make_scheme_stream scheme_read_from_file);
    1;
}

our %READ_TABLE = (
		   '(' =>
		   sub {
		       my($stream, $char, $term_char) = @_;
		       return scheme_read_delimited_list($stream, ')');
		   },
		   "'" =>
		   sub {
		       my($stream, $char, $term_char) = @_;
		       my $thing = scheme_read($stream, $term_char);
		       return ['quote', $thing];
		   },
		   '"' =>
		   sub {
		       my($stream, $char, $term_char) = @_;
		       my $str = '';
		       my $unescaped = 1;
		       until($stream->('peek') eq '"' and $unescaped) {
			   my $chr = $stream->('read', 1);
			   if ($chr eq "\\") {
			       $unescaped = 0 if $unescaped;
			   }
			   else {
			       $unescaped = 1;
			   }
			   $str .= $chr;
		       }
		       $stream->('read', 1);
		       return ['string', $str];
		   },
		   ';' => sub {
		       my $stream = shift;
		       while ($stream->('peek') ne "\n") {
			   $stream->('read', 1);
		       }
		       return undef;
		   },
		  );

our $EOF = 0;

sub scheme_read {
    my $stream = shift || make_scheme_stream(\*STDIN);
    my $term_char = shift || "\0";

    unless (ref $stream eq "CODE") {
	$stream = make_scheme_stream($stream);
    }

    my $obj;

  LOOP: {
	my $char = $stream->('peek');
	last LOOP unless $char;

	if (exists $READ_TABLE{$char}) {
	    $stream->('read', 1);
	    return $READ_TABLE{$char}->($stream, $char, $term_char);
	}
	elsif ($stream->('eof')) {
	    last LOOP;
	}
	elsif ($char !~ /[\w\-!$%@^&*_+=\[\]\{\}:<>?\/#\.]/) {
	    $stream->('read', 1) unless $char eq $term_char;
	    last LOOP if defined($obj);
	}
	else {
	    $stream->('read', 1);
	    $obj .= $char;
	    redo LOOP;
	}
    }
    return $obj;
}

sub scheme_read_from_file {
    my $stream = shift;
    my @lst = ();

    unless (ref $stream eq "CODE") {
	$stream = make_scheme_stream($stream);
    }

    until ($stream->('eof')) {
	my $thing = scheme_read($stream);
	push @lst, $thing if defined($thing);
    }

    return \@lst;    
}

sub scheme_read_delimited_list {
    my ($stream, $term_char) = @_;
    my @lst = ();

    unless (ref $stream eq "CODE") {
	$stream = make_scheme_stream($stream);
    }

    my $char = $stream->('peek');
    while ($char ne $term_char) {
	my $thing = scheme_read($stream, $term_char);
	push @lst, $thing if defined($thing);
	$char = $stream->('peek');
    }

    $stream->('read', 1);
    return \@lst;
}

sub make_scheme_stream {
    my $stream = shift;
    my $buffer = '';
    my %options = ();
    %options = (
		'read' => sub {
		    my $acc = '';
		    my $length = shift // 1;
		    if ($buffer) {
			while ($buffer and ($length >= 1)) {
			    $acc .= substr($buffer, 0, 1);
			    $buffer = substr($buffer, 1);
			    $length--;
			}
		    }
		    if ($length) {
			my $tmp = '';
			unless( read $stream, $tmp, $length ) { $EOF = 1; }
			$acc .= $tmp;
		    }
		    return $acc;
		},
		'eof' => sub {
		    my $char = $options{peek}->();
		    if (eof($stream) or ! defined($char)) {
			return 1;
		    }
		    return 0;
		},
		to_string => sub {
		    return "Buffer: $buffer\nStream: $stream";
		},
		peek => sub {
		    if ($buffer) {
			return substr($buffer, 0, 1);
		    }
		    else {
			read $stream, $buffer, 1;
			return $buffer;
		    }
		},
	       );
    return sub {
	my $op = shift;
	if (exists $options{$op}) {
	    $options{$op}->(@_);
	}
	else {
	    my ($pkg, $file, $line) = caller;
	    die "Error in read: File $file, package $pkg, at line $line: no subroutine \"$op\"\n";
	}
    }
}

1;

__END__

=head1 NAME

Reader -- A suite of functions to read LISP data

=head1 SYNOPSIS

   use Data::Dumper;
   use Reader;

   my $array_ref = scheme_read(\*STDIN);
   print "LISP structure read in: " . Dumper($array_ref) . "\n";

=head1 DESCRIPTION

This is a collection of functions that allows you to read LISP like
data from a Perl data stream.

=head1 FUNCTIONS

These functions are automatically imported into the current package,
so you can call them directly. (The method for doing this was snarfed
from L<Quantum::Superpositions>.)

=over 4

=item scheme_read

Takes a file handle of some sort.

=item make_scheme_stream

Given a file handle, this returns a stream that L</scheme_read> can
use. Rarely called directly. L</scheme_read> will automatically call
this function if it is passed a raw Perl file handle.

=item scheme_read_delimited_list

Takes a file handle and a terminating character. This will try to read
objects from the stream until the next character on the stream is the
terminating char. Returns an array ref of all objects read.

=item scheme_read_from_file

Given a file handle, will continuously call L</scheme_read> on the file handle until the file's contents have been exhausted.

=back

=head1 EXAMPLES

A one-time use REPL: (just the Read part)

   use Data::Dumper;
   use Reader;
   
   print "Expr: ";
   my $thing = scheme_read(0);
   print "Thing: " . Dumper($thing) . "\n";


Reading data from a file:

   use Data::Dumper;
   use Reader;
   
   open my $fh, '<', $file or die "open: $!";
   
   my $data = scheme_read_from_file($fh);
   print "From file:\n" . Dumper($data) . "\n";


=head1 AUTHOR

Ashton Wiersdorf <ashton.wiersdorf@mailblock.net>
