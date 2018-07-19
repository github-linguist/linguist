use 5.01;
use Memoize;

sub cc {
    my $amount = shift;
    return 0 if !@_ || $amount < 0;
    return 1 if $amount == 0;
    my $first = shift;
    cc( $amount, @_ ) + cc( $amount - $first, $first, @_ );
}
memoize 'cc';

# Make recursive algorithm run faster by sorting coins descending by value:
sub cc_optimized {
    my $amount = shift;
    cc( $amount, sort { $b <=> $a } @_ );
}

say 'Ways to change $ 1 with common coins: ',
    cc_optimized( 100, 1, 5, 10, 25 );
say 'Ways to change $ 1000 with addition of less common coins: ',
    cc_optimized( 1000 * 100, 1, 5, 10, 25, 50, 100 );
