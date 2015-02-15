use strict;

sub halve { int((shift) / 2); }
sub double { (shift) * 2; }
sub iseven { ((shift) & 1) == 0; }

sub ethiopicmult
{
    my ($plier, $plicand, $tutor) = @_;
    print "ethiopic multiplication of $plier and $plicand\n" if $tutor;
    my $r = 0;
    while ($plier >= 1)
    {
	$r += $plicand unless iseven($plier);
	if ($tutor) {
	    print "$plier, $plicand ", (iseven($plier) ? " struck" : " kept"), "\n";
	}
	$plier = halve($plier);
	$plicand = double($plicand);
    }
    return $r;
}

print ethiopicmult(17,34, 1), "\n";
