my @f = ([],(map {chomp;['',( split // ),'']} <>),[]);

for (1 .. 10) {
	print join "", map {"@$_\n"} @f;
	my @a = ([]);
	for my $y (1 .. $#f-1) {
		my $r = $f[$y];
		my $rr = [''];
		for my $x (1 .. $#$r-1) {
			my $c = $r->[$x];
			push @$rr,
				$c eq 'H' ? 't' :
				$c eq 't' ? '.' :
				$c eq '.' ? (join('', map {"@{$f[$_]}[$x-1 .. $x+1]"=~/H/g} ($y-1 .. $y+1)) =~ /^H{1,2}$/ ? 'H' : '.') :
				$c;
		}
		push @$rr, '';
		push @a, $rr;
	}
	@f = (@a,[]);
}
