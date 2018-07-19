sub compose {
    my ($f, $g) = @_;

    sub {
        $f -> ($g -> (@_))
    };
}

use Math::Trig;
print compose(sub {sin $_[0]}, \&asin)->(0.5), "\n";
