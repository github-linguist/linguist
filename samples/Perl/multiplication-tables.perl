our $max = 12;
our $width = length($max**2) + 1;

printf "%*s", $width, $_ foreach 'x|', 1..$max;
print "\n", '-' x ($width - 1), '+', '-' x ($max*$width), "\n";
foreach my $i (1..$max) {
	printf "%*s", $width, $_
            foreach "$i|", map { $_ >= $i and $_*$i } 1..$max;
	print "\n";
}
