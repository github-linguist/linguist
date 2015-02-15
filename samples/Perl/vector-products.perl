package Vector;
use List::Util 'sum';
use List::MoreUtils 'pairwise';

sub new { shift; bless [@_] }

use overload (
        '""'    => sub { "(@{+shift})" },
        '&'     => sub { sum pairwise { $a * $b } @{+shift}, @{+shift} },
        '^'     => sub {
                                my @a = @{+shift};
                                my @b = @{+shift};
                                bless [ $a[1]*$b[2] - $a[2]*$b[1],
                                        $a[2]*$b[0] - $a[0]*$b[2],
                                        $a[0]*$b[1] - $a[1]*$b[0] ]
                        },
);

package main;
my $a = Vector->new(3, 4, 5);
my $b = Vector->new(4, 3, 5);
my $c = Vector->new(-5, -12, -13);

print "a = $a b = $b c = $c\n";
print "$a . $b = ", $a & $b, "\n";
print "$a x $b = ", $a ^ $b, "\n";
print "$a . ($b x $c) = ", $a & ($b ^ $c), "\n";
print "$a x ($b x $c) = ", $a ^ ($b ^ $c), "\n";
