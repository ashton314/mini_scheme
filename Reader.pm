package Reader;
use strict;
use warnings;

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
		  );

our $STDIN = make_scheme_stream(\*STDIN);
our $EOF = 0;

sub scheme_read {
    my $stream = shift || $STDIN;
    my $term_char = shift // "";
    my $pending_obj = '';

  LOOP: {
	my $char = $stream->('peek');

	if ($char) {
	    if (exists $READ_TABLE{$char}) {
		$stream->('read', 1);
		return $READ_TABLE{$char}->($stream, $char, $term_char);
	    }
	    elsif ($char eq $term_char) {
		$stream->('read', 1);
		return wantarray ? ($pending_obj, 1) : $pending_obj;
	    }
	    elsif ($EOF or ($char eq ' ')) {
		$stream->('read', 1);
		return wantarray ? ($pending_obj, 0) : $pending_obj;
	    }
	    else {
		$pending_obj .= $stream->('read', 1);
	    }
	    redo LOOP;
	}
    }
}

sub scheme_read_delimited_list {
    my($stream, $term_char) = @_;
    my @things = ();
    my $last_was_list = 0;
  LOOP: {
	my($thing, $eol) = scheme_read($stream, $term_char);
	push @things, $thing unless $last_was_list;
	$last_was_list = (ref $thing eq 'ARRAY');
	redo LOOP unless $eol;
    }

    return \@things;
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
