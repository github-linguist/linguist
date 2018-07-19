package One_To_Ten;
use Carp qw(croak);
use Tie::Scalar qw();
use base qw(Tie::StdScalar);

sub STORE {
    my $self = shift;
    my $val = int shift;
    croak 'out of bounds' if $val < 1 or $val > 10;
    $$self = $val;
};

package main;
tie my $t, 'One_To_Ten';
$t = 3;   # ok
$t = 5.2; # ok, silently coerced to int
$t = -2;  # dies, too small
$t = 11;  # dies, too big
$t = 'xyzzy';
# dies, too small. string is 0 interpreted numerically
