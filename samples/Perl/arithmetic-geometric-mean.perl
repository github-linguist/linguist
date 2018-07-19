#!/usr/bin/perl -w

my ($a0, $g0, $a1, $g1);

sub agm($$) {
    $a0 = shift;
    $g0 = shift;
    do {
        $a1 = ($a0 + $g0)/2;
        $g1 = sqrt($a0 * $g0);
        $a0 = ($a1 + $g1)/2;
        $g0 = sqrt($a1 * $g1);
    } while ($a0 != $a1);
    return $a0;
}

print agm(1, 1/sqrt(2))."\n";
