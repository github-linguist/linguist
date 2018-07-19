use strict;
use warnings;

my $x = my $y = 255;
$x |= 1; # must be odd
my $depth = 255;

my $light = Vector->new(rand, rand, rand)->normalized;

print "P2\n$x $y\n$depth\n";

my ($r, $ambient) = (($x - 1)/2, 0);
my ($r2) = $r ** 2;
{
    for my $x (-$r .. $r) {
	my $x2 = $x**2;
	for my $y (-$r .. $r) {
	    my $y2 = $y**2;
	    my $pixel = 0;
	    if ($x2 + $y2 < $r2) {
		my $v = Vector->new($x, $y, sqrt($r2 - $x2 - $y2))->normalized;
		my $I = $light . $v + $ambient;
		$I = $I < 0 ? 0 : $I > 1 ? 1 : $I;
		$pixel = int($I * $depth);
	    }
	    print $pixel;
	    print $y == $r ? "\n" : " ";
	}
    }
}

package Vector {
    sub new {
	my $class = shift;
	bless ref($_[0]) eq 'Array' ? $_[0] : [ @_ ], $class;
    }
    sub normalized {
	my $this = shift;
	my $norm = sqrt($this . $this);
	ref($this)->new( map $_/$norm, @$this );
    }
    use overload q{.} => sub {
	my ($a, $b) = @_;
	my $sum = 0;
	for (0 .. @$a - 1) {
	    $sum += $a->[$_] * $b->[$_]
	}
	return $sum;
    },
    q{""} => sub { sprintf "Vector:[%s]", join ' ', @{shift()} };
}
