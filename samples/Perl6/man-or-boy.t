use Test;

# stress test for lexicals and lexical subs
# See 
# http://en.wikipedia.org/w/index.php?title=Man_or_boy_test&oldid=249795453#Perl

my @results = 1, 0, -2, 0, 1, 0, 1, -1, -10, -30;

# if we want to *really* stress-test, we can use a few more tests:
# my @results = 1, 0, -2, 0, 1, 0, 1, -1, -10, -30, -67, -138
# -291, -642, -1446, -3250, -7244, -16065, -35601, -78985;

plan +@results;

sub A($k is copy, &x1, &x2, &x3, &x4, &x5) {
    my $B;
    $B = sub (*@) { A(--$k, $B, &x1, &x2, &x3, &x4) };
    if ($k <= 0) {
        return    x4($k, &x1, &x2, &x3, &x4, &x5)
                + x5($k, &x1, &x2, &x3, &x4, &x5);
    }
    return $B();
};

for 0 .. (@results-1) -> $i {
    is A($i, sub (*@) {1}, sub (*@) {-1}, sub (*@) {-1}, sub (*@) {1}, sub (*@) {0}),
       @results[$i],
       "man-or-boy test for start value $i";
}


# vim: ft=perl6
