use 5.010;

use List::Util qw/max sum/;

sub fib {
    my $n = shift;
    my $xs = shift // [1];
    my @xs = @{$xs};

    while (my $len = scalar @xs) {
        last if $len >= 20;
        push(
            @xs,
            sum(@xs[max($len - $n, 0)..$len-1])
        );
    }

    return @xs;
}

for (2..10) {
    say join(' ', fib($_));
}
say join(' ', fib(2, [2,1]));
