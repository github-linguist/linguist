use strict;
package SquareMatrix;
use Carp;                       # standard, "it's not my fault" module

use overload (
        '""'    => \&_string,   # overload string operator so we can just print
        '*'     => \&_mult,     # multiplication, needed for expo
        '*='    => \&_mult,     # ditto, explicitly defined to trigger copy
        '**'    => \&_expo,     # overload exponentiation
        '='     => \&_copy,     # copy operator
);

sub make {
        my $cls = shift;
        my $n = @_;
        for (@_) {
                # verify each row given is the right length
                confess "Bad data @$_: matrix must be square "
                        if @$_ != $n;
        }

        bless [ map [@$_], @_ ] # important: actually copy all the rows
}

sub identity {
        my $self = shift;
        my $n = @$self - 1;
        my @rows = map [ (0) x $_, 1, (0) x ($n - $_) ], 0 .. $n;
        bless \@rows
}

sub zero {
        my $self = shift;
        my $n = @$self;
        bless [ map [ (0) x $n ], 1 .. $n ]
}

sub _string {
        "[ ".join("\n  " =>
                map join(" " => map(sprintf("%12.6g", $_), @$_)), @{+shift}
        )."  ]\n";
}

sub _mult {
        my ($a, $b) = @_;
        my $x = $a->zero;
        my @idx = (0 .. $#$x);
        for my $j (@idx) {
                my @col = map($a->[$_][$j], @idx);
                for my $i (@idx) {
                        my $row = $b->[$i];
                        $x->[$i][$j] += $row->[$_] * $col[$_] for @idx;
                }
        }
        $x
}

sub _expo {
        my ($self, $n) = @_;
        confess "matrix **: must be non-negative integer power"
                        unless $n >= 0 && $n == int($n);

        my ($tmp, $out) = ($self, $self->identity);
        do {
                $out *= $tmp    if $n & 1;
                $tmp *= $tmp;
        } while $n >>= 1;

        $out
}

sub _copy { bless [ map [ @$_ ], @{+shift} ] }

# now use our matrix class
package main;

my $m = SquareMatrix->make(
                [1, 2, 0],
                [0, 3, 1],
                [1, 0, 0] );
print "### Order $_\n", $m ** $_        for 0 .. 10;

$m = SquareMatrix->make(
        [ 1.0001, 0,      0, 1       ],
        [ 0,      1.001,  0, 0       ],
        [ 0,      0,      1, 0.99998 ],
        [ 1e-8,   0,      0, 1.0002  ]);

print "\n### Matrix is now\n",  $m;
print "\n### Big power:\n",     $m ** 100_000;
print "\n### Too big:\n",       $m ** 1_000_000;
print "\n### WAY too big:\n",   $m ** 1_000_000_000_000;
print "\n### But identity matrix can handle that\n",
                $m->identity ** 1_000_000_000_000;
