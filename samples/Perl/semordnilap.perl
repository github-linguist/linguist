while (<>) {
	chomp;
	my $r = reverse;
	$seen{$r}++ and $c++ < 5 and print "$_ $r\n" or $seen{$_}++;
}

print "$c\n"
