package Reader;
use strict;
use warnings;

sub import {
    my $caller = caller;
    no strict 'refs';
    *{ $caller . '::' . $_ } = __PACKAGE__->can( $_ )
      for qw(scheme_read scheme_read_delimited_list make_scheme_stream);
    1;
}

our %READ_TABLE = (
		   '(' =>
		   sub {
		       my($stream, $char) = @_;
		       return scheme_read_delimited_list($stream, ')');
		   },
		   "'" =>
		   sub {
		       my($stream, $char) = @_;
		       my $thing = scheme_read($stream);
		       return ['quote', $thing];
		   },
		   '"' =>
		   sub {
		       my($stream, $char) = @_;
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
		       return $str;
		   },
		  );

our $STDIN = make_scheme_stream(\*STDIN);
our $EOF = 0;

sub scheme_read {
    my $stream = shift || $STDIN;
    my $term_char = shift || "\0";

    my $obj;

  LOOP: {
	my $char = $stream->('peek');
#	print "Char in loop: `$char`\n";
	if (exists $READ_TABLE{$char}) {
	    $stream->('read', 1);
	    return $READ_TABLE{$char}->($stream, $char);
	}
	elsif ($stream->('eof')) {
	    last LOOP;
	}
	elsif ($char !~ /\w/) {
	    $stream->('read', 1) unless $char eq $term_char;
	    last LOOP if $obj;
	}
	else {
	    $stream->('read', 1);
	    $obj .= $char;
	    redo LOOP;
	}
    }
    return $obj;
}

sub scheme_read_delimited_list {
    my ($stream, $term_char) = @_;
#    print "Term char: $term_char\n";
    my @lst = ();

    my $char = $stream->('peek');
    while ($char ne $term_char) {
#	print "Char in while: `$char`\n";
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
    my %options = (
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
		       return eof($stream);
		   },
		   to_string => sub {
		       return "Buffer: $buffer\nStream: $stream";
		   },
		   'peek' => sub {
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

   my $array_ref = Reader::scheme_read(\*STDIN, "\n");
   print "LISP structure read in: " . Dumper($array_ref) . "\n";

=head1 DESCRIPTION

This is a collection of functions that allows you to read LISP like
data from a Perl data stream.

=head1 FUNCTIONS

=over 4

=item scheme_read($scheme_stream, $eof_char)

Takes a file handle and a character considered to be the "end of file"
character. The EOF char defaults to ^D. For making your own REPL, try
using "\n" as the EOF character.

This returns an array ref.

B<$scheme_stream> must be something produced by
L</make_scheme_stream>. If it is `0' (or some other false value), the
stream will default to B<STDIN>.

In the future, I might make this so that it will simply take a file
handle as its argument.

=item make_scheme_stream($fh)

Given a file handle, this returns a stream that L</scheme_read> can
use. B<IT IS VERY IMPORTANT THAT THIS FUNCTION BE USED!> This is
required even when slurping from a file.

=back

=head1 AUTHOR

Ashton Wiersdorf <ashton.wiersdorf@mailblock.net>
