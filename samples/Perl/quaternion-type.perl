package Quaternion;
use List::Util 'reduce';
use List::MoreUtils 'pairwise';

sub make {
        my $cls = shift;
        if (@_ == 1)    { return bless [ @_, 0, 0, 0 ] }
        elsif (@_ == 4) { return bless [ @_ ] }
        else            { die "Bad number of components: @_" }
}

sub _abs { sqrt reduce { $a + $b * $b } @{ +shift } }
sub _neg { bless [ map(-$_, @{+shift}) ] }
sub _str { "(@{+shift})" }

sub _add {
        my ($x, $y) = @_;
        $y = [ $y, 0, 0, 0 ] unless ref $y;
        bless [ pairwise { $a + $b } @$x, @$y ]
}

sub _sub {
        my ($x, $y, $swap) = @_;
        $y = [ $y, 0, 0, 0 ] unless ref $y;
        my @x = pairwise { $a - $b } @$x, @$y;
        if ($swap) { $_ = -$_ for @x }
        bless \@x;
}

sub _mul {
        my ($x, $y) = @_;
        if (!ref $y) { return bless [ map($_ * $y, @$x) ] }
        my ($a1, $b1, $c1, $d1) = @$x;
        my ($a2, $b2, $c2, $d2) = @$y;
        bless [ $a1 * $a2 - $b1 * $b2 - $c1 * $c2 - $d1 * $d2,
                $a1 * $b2 + $b1 * $a2 + $c1 * $d2 - $d1 * $c2,
                $a1 * $c2 - $b1 * $d2 + $c1 * $a2 + $d1 * $b2,
                $a1 * $d2 + $b1 * $c2 - $c1 * $b2 + $d1 * $a2]
}

sub conjugate {
        my @a = map { -$_ } @{$_[0]};
        $a[0] = $_[0][0];
        bless \@a
}

use overload (
        '""'    => \&_str,
        '+'     => \&_add,
        '-'     => \&_sub,
        '*'     => \&_mul,
        'neg'   => \&_neg,
        'abs'   => \&_abs,
);

package main;

my $a = Quaternion->make(1, 2, 3, 4);
my $b = Quaternion->make(1, 1, 1, 1);

print "a = $a\n";
print "b = $b\n";
print "|a| = ", abs($a), "\n";
print "-a = ", -$a, "\n";
print "a + 1 = ", $a + 1, "\n";
print "a + b = ", $a + $b, "\n";
print "a - b = ", $a - $b, "\n";
print "a conjugate is ", $a->conjugate, "\n";
print "a * b = ", $a * $b, "\n";
print "b * a = ", $b * $a, "\n";
