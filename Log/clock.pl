#!/usr/bin/env perl
use strict;
use warnings;

use Storable;
use File::Copy;

my $file = "workLog.data";
open my $tempfh, '<', $file;
my %times = ();
if (<$tempfh>) {
    %times = %{ retrieve($file) };
}
close $tempfh;

my $start;		# Non-local so the SIGtrap can use it
my $stop;		# ditto

my $in_run = 0;

my %commands = (
		run     => sub { enter(run()); },
		start   => sub { enter(run()); },
		load    => sub { load(); },
		restore => sub { restore(); },
		help    => sub { help(); },
		h       => sub { help(); },
		'?'     => sub { help(); },
		finish  => sub { exit; },
		end     => sub { exit; },
		quit    => sub { exit; },
		exit    => sub { exit; },
		term    => sub { terminate(); },
		html    => sub { html(); },
		'dump'  => sub { dumper(1); },
    );

LOOP: {
    print "> ";
    chomp(my $command = <STDIN>);
    if ( exists $commands{$command} ) {
	$commands{$command}->();
    }
    else {
	print "Unknown command: $command\nPlease type `h' for help.\n";
    }
    redo LOOP;
}

sub run {
    $in_run = 1;
    $start = '';
    $stop = '';
    print "Press enter to start...";
    <STDIN>;
    $start = time;
    print "Started at " . localtime . ". Press enter to stop...";
    <STDIN>;
    $stop = time;
    print "Stopped at " . localtime . ".\n";
    $in_run = 0;
    return ($start, $stop);
}

sub enter {
    my ($start, $stop) = @_;
    $times{$start} = $stop;
#    map { print "$_ :: $times{$_}\n" } keys %times;
    store(\%times, $file);
}

sub load {
    store(\%times, $file);
    my @files = glob 'WorkLog/*';
    print "Files:\n";
    while(@files) {
	printf("%20s %20s %20s\n", (shift @files || ''),
	       (shift @files || ''), (shift @files || ''));
    }
    print "File to load: ";
    chomp(my $file2 = <STDIN>);
    $file2 = "WorkLog/$file2";
    unless (-e $file2) {
	print "404: File not found\n";
	load();
    }
    %times = %{ retrieve($file2) } or warn "Could not retrieve data: $!";
    close $tempfh;

}

sub restore {
    print "Really overwrite data? [y/N]: ";
    chomp(my $resp = <STDIN>);
    unless($resp eq 'y') {
	print "Aborting.\n";
    }
    load();
    store(\%times, $file);
}

sub terminate {
    dumper(1);			# will append the total value
    store(\%times, $file);
    copy($file, "WorkLog/$file" . time);
    open my $fh, '>', $file;
    print $fh '';
    close $fh;
    exit;
}

sub dumper {
    my @starts = sort keys %times;
    my @stops = map {$times{$_}} @starts;
    my @starts2 = sort keys %times;
    my @stops2 = map {$times{$_}} @starts;

    my $start_time = scalar(localtime(min(keys %times)));
    my $end_time   = scalar(localtime(max(values %times)));

    print <<__HEAD__;
Work log for: Ashton Wiersdorf
Start Date  : $start_time
End Date    : $end_time
+----------------------------+----------------------------+------------+
|           Start            |            End             |   Total    |
+----------------------------+----------------------------+------------+
__HEAD__

    while(@stops && @starts) {
	format STDOUT =
| @||||||||||||||||||||||||| | @||||||||||||||||||||||||| | @||||||||| |
scalar(localtime($starts[0])), scalar(localtime($stops[0])), pretty((shift @stops) - (shift @starts))
+----------------------------+----------------------------+------------+
.
	write;
    }

    printf "|        TOTAL TIME          |                            |  %-9s |\n",
      scalar(stringify(\@starts2, \@stops2));
    print "+----------------------------+----------------------------+------------+\n";
}

sub html {
    my @starts = sort keys %times;
    my @stops = map {$times{$_}} @starts;
    my @starts2 = sort keys %times;
    my @stops2 = map {$times{$_}} @starts;

    my $start_time = scalar(localtime(min(keys %times)));
    my $end_time   = scalar(localtime(max(values %times)));

    print <<__HEAD__;
<table>
  <tr>
    <td><b>Work log for:</b></td><td>Ashton Wiersdorf</td>
  <tr/>
  <tr>
    <td><b>Start Date:</b></td><td>$start_time</td>
  </tr>
  <tr>
    <td><b>End Date:</b></td><td>$end_time</td>
  </tr>
</table>

<table>
  <thead>
    <tr><th>Start</th><th>End</th><th>Total</th></tr>
  </thead>
  <tbody>
__HEAD__

    while(@stops && @starts) {
	print "<tr><td>" . scalar(localtime($starts[0])) . "</td><td>" . scalar(localtime($stops[0])) . "</td><td>" . pretty((shift @stops) - (shift @starts)) . "</tr>\n";
    }
    print "<tr><td>TOTAL TIME</td><td></td><td>";
    print scalar(stringify(\@starts2, \@stops2));
    print "</td></tr></tbody></table>\n";
}

sub help {
    print <<__HELP__;
run     -- start a recording session
h       -- this page
end     -- stop program
dump    -- print the table
term    -- print the table, archive the log,
           and make a new one
load    -- load an old record file
restore -- store an old record file as the
           working file.
__HELP__

}

sub min {
    my $ret  = shift;
    my @vals = @_;
    map {$ret = $_ < $ret ? $_ : $ret} @vals;
    return $ret;
}

sub max {
    my $ret  = shift;
    my @vals = @_;
    map {$ret = $_ > $ret ? $_ : $ret} @vals;
    return $ret;
}

sub stringify {
    # Take the starts and the stops and return a pretty string of the total
    my ($strts, $stps) = @_;
    my @starts = @{$strts};
    my @stops  = @{$stps};

    my $total = 0;
    while(@starts) {
	$total += ((shift @stops) - (shift @starts));
    }

    return pretty($total, wantarray);
}

sub pretty {
    # Takes number of seconds and returns a nice string/list (depending on context)
    my $total    = shift;
    my $wantlist = shift;

    my $seconds = $total % 60;
    my $minutes = int $total / 60;
    my $hours   = int $minutes / 60;
    $minutes    %= 60;

    return $wantlist ? ($hours, $minutes, $seconds): "$hours:$minutes:$seconds";
}
