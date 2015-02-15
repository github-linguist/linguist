use Math::Complex;

($x1,$x2) = solveQuad(1,2,3);

print "x1 = $x1, x2 = $x2\n";

sub solveQuad
{
	my ($a,$b,$c) = @_;
	my $root = sqrt($b**2 - 4*$a*$c);
	return ( -$b + $root )/(2*$a), ( -$b - $root )/(2*$a);
}
