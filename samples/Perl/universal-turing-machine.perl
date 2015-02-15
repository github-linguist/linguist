use strict;
use warnings;

sub run_utm {
	my %o = @_;
	my $st = $o{state}	// die "init head state undefined";
	my $blank = $o{blank}	// die "blank symbol undefined";
	my @rules = @{$o{rules}} or die "rules undefined";
	my @tape = $o{tape} ? @{$o{tape}} : ($blank);
	my $halt = $o{halt};

	my $pos = $o{pos} // 0;
	$pos += @tape if $pos < 0;
	die "bad init position" if $pos >= @tape || $pos < 0;

step:	while (1) {
		print "$st\t";
		for (0 .. $#tape) {
			my $v = $tape[$_];
			print $_ == $pos ? "[$v]" : " $v ";
		}
		print "\n";

		last if $st eq $halt;
		for (@rules) {
			my ($s0, $v0, $v1, $dir, $s1) = @$_;
			next unless $s0 eq $st and $tape[$pos] eq $v0;

			$tape[$pos] = $v1;

			if ($dir eq 'left') {
				if ($pos == 0) { unshift @tape, $blank}
				else { $pos-- }
			} elsif ($dir eq 'right') {
				push @tape, $blank if ++$pos >= @tape
			}

			$st = $s1;
			next step;
		}

		die "no matching rules";
	}
}

print "incr machine\n";
run_utm	halt=>'qf',
	state=>'q0',
	tape=>[1,1,1],
	blank=>'B',
	rules=>[[qw/q0 1 1 right q0/],
		[qw/q0 B 1 stay  qf/]];

print "\nbusy beaver\n";
run_utm halt=>'halt',
	state=>'a',
	blank=>'0',
	rules=>[[qw/a 0 1 right b/],
		[qw/a 1 1 left  c/],
		[qw/b 0 1 left  a/],
		[qw/b 1 1 right b/],
		[qw/c 0 1 left  b/],
		[qw/c 1 1 stay  halt/]];

print "\nsorting test\n";
run_utm halt=>'STOP',
	state=>'A',
	blank=>'0',
	tape=>[qw/2 2 2 1 2 2 1 2 1 2 1 2 1 2/],
	rules=>[[qw/A 1 1 right A/],
		[qw/A 2 3 right B/],
		[qw/A 0 0 left  E/],
		[qw/B 1 1 right B/],
		[qw/B 2 2 right B/],
		[qw/B 0 0 left  C/],
		[qw/C 1 2 left  D/],
		[qw/C 2 2 left  C/],
		[qw/C 3 2 left  E/],
		[qw/D 1 1 left  D/],
		[qw/D 2 2 left  D/],
		[qw/D 3 1 right A/],
		[qw/E 1 1 left  E/],
		[qw/E 0 0 right STOP/]];
