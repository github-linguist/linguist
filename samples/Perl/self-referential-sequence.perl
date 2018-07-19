sub next_num {
	my @a;
	$a[$_]++ for split '', shift;
	join('', map(exists $a[$_] ? $a[$_].$_ : "", reverse 0 .. 9));
}

my %cache;
sub seq {
	my $a = shift;
	my (%seen, @s);
	until ($seen{$a}) {
		$seen{$a} = 1;
		push(@s, $a);
		last if !wantarray && $cache{$a};
		$a = next_num($a);
	}
	return (@s) if wantarray;

	my $l = $cache{$a};
	if ($l) { $cache{$s[$_]} = $#s - $_ + $l for (0 .. $#s); }
	else {
		$l++ while ($s[-$l] != $a);
		$cache{pop @s} = $l 	for (1 .. $l);
		$cache{pop @s} = ++$l	while @s;
	}
	$cache{$s[0]}
}

my (@mlist, $mlen);
for (1 .. 100_000) { # 1_000_000 takes very, very long
	my $l = seq($_);
	next if $l < $mlen;

	if ($l > $mlen) { $mlen = $l; @mlist = (); }
	push @mlist, $_;
}

print "longest ($mlen): @mlist\n";
print join("\n", seq($_)), "\n\n"	for @mlist;
