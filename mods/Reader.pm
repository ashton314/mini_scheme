package Reader;
use strict;
use warnings;

our $VERSION = "2.0.3";

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
			   $unescaped = $chr eq "\\" ? ! $unescaped : 1;
			   $str .= $chr;
		       }
		       $stream->('read', 1);
		       return ['string', $str];
		   },
		   ';' => sub {
		       my $stream = shift;
		       until ($stream->('peek') eq "\n" or $stream->('eof')) {
			   $stream->('read', 1);
		       }
		       return undef;
		   },
		   '`' => sub {
		       my ($stream, $char, $term_char) = @_;
		       my $thing = scheme_read($stream, $term_char);
		       return ['backquote', $thing];
		   },
		   ',' => sub {
		       my ($stream, $char, $term_char) = @_;
		       my $nchar = $stream->('peek');
		       if ($nchar eq '@') {
			   $stream->('read', 1);
			   my $thing = scheme_read($stream, $term_char);
			   return ['comma-splice', $thing];
		       }
		       else {
			   my $thing = scheme_read($stream, $term_char);
			   return ['comma', $thing];
		       }
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
	last LOOP unless length $char;

	if (exists $READ_TABLE{$char}) {
	    $stream->('read', 1);
	    return $READ_TABLE{$char}->($stream, $char, $term_char);
	}
	elsif ($char !~ /[\w\-\!\$\%\@\^\&\*\_\+\=\[\]\{\}\:\<\>\?\/\#\.]/ ) {
	    $stream->('read', 1) unless ($char eq $term_char);
	    last LOOP if defined($obj);
	}
	else {
	    $stream->('read', 1);
	    $obj .= $char;
	    redo LOOP;
	}
    }

    my $zerop = 0;
    {
	no warnings;
	if ($obj eq '0') {
	    $zerop = 1;
	}
    }

    if (wantarray) {
	return ($obj, $zerop);
    }
    else {
	return $obj;
    }
}

sub scheme_read_delimited_list {
    my ($stream, $term_char) = @_;
    my @lst = ();

    unless (ref $stream eq "CODE") {
	$stream = make_scheme_stream($stream);
    }

    my $char = $stream->('peek');
    while ($char ne $term_char) {
	my ($thing, $zerop) = scheme_read($stream, $term_char);
	if ($zerop) {
	    push @lst, '0';
	}
	elsif (defined($thing)) {
	    push @lst, $thing;
	}
	$char = $stream->('peek');
    }

    $stream->('read', 1);
    return \@lst;
}

sub scheme_read_from_file {
    my $stream = shift;
    my @lst = ();

    unless (ref $stream eq "CODE") {
	$stream = make_scheme_stream($stream);
    }

    until ($stream->('eof')) {
	my $thing = scheme_read($stream);
	push @lst, $thing;
    }

    @lst = grep { defined } @lst;
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
		    if (length $buffer) {
			while (length $buffer and ($length >= 1)) {
			    $acc .= substr($buffer, 0, 1);
			    $buffer = substr($buffer, 1);
			    $length--;
			}
		    }
		    if ($length) {
			my $tmp = '';
			unless( read $stream, $tmp, $length ) { $EOF = 1; }
			$acc .= '' . $tmp;
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
		    if ($buffer ne '') {
			return '' . substr($buffer, 0, 1);
		    }
		    else {
			read $stream, $buffer, 1;
			return '' . $buffer;
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

   my $array_ref = scheme_read(0, "\n");
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

Takes a file handle and a character that will terminate a lisp expression. The file handle defaults to STDIN. An argument of 0 will trigger the default. For REPLs, use "\n" as the terminating character.

This returns either an array ref or a scalar value, depending on what was read. Example:

  * foo
  Dumper: $VAR1 = 'foo';
  * (foo zoop narf)
  Dumper: $VAR1 = ['foo', 'zoop', 'narf'];
  * (foo ((zoop) narf) bar (baz))
  Dumper: $VAR1 = ['foo', [['zoop'], 'narf'], 'bar', ['baz']];

=item make_scheme_stream

Given a file handle, this returns a stream that L</scheme_read> can
use. Rarely called directly. L</scheme_read> will automatically call
this function if it is passed a raw Perl file handle.

=item scheme_read_delimited_list

Takes a file handle and a terminating character. This will try to read
objects from the stream until the next character on the stream is the
terminating char. Returns an array ref of all objects read.

=item scheme_read_from_file

Given a file handle, will continuously call L</scheme_read> on the file handle until the file's contents have been exhausted. If the file does not have a newline as the final character, the function may hang.

=back

=head1 READ MACROS

All the read macros for this reader are stored in the variable C<%Reader::READ_TABLE>. The keys of this hash are one-character strings that identify the macro character. The value of the pairs is a subroutine that takes the current stream object, the macro character, and the terminating character (originally passed to L</scheme_read>.) The stream does B<NOT> have the macro character at the beginning.

Unfortunatly, dispatching macro characters have not been implemented. Fortunatly, they should not be too dificult to implement. This might be possible to do without modifying Reader.pm

=head1 EXAMPLES

A simple REPL:

   use Data::Dumper;
   use Reader;

   while (1) {
      print "* ";
      my $thing = scheme_read(0, "\n");
      print "Thing: " . Dumper($thing) . "\n";
   }


Reading data from a file:

   use Data::Dumper;
   use Reader;

   open my $fh, '<', $file or die "open: $!";

   my $data = scheme_read_from_file($fh);
   print "From file:\n" . Dumper($data) . "\n";

=head1 BUGS

=over 4

=item *

No built-in support for dispatching macro characters.

=back

=head1 AUTHOR

Ashton Wiersdorf <ashton.wiersdorf@mailblock.net>
