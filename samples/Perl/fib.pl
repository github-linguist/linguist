#! perl
# Copyright (C) 2001-2003, Parrot Foundation.

=head1 NAME

examples/benchmarks/fib.pl - Fibonacci Benchmark

=head1 SYNOPSIS

    % time perl examples/benchmarks/fib.pl n

=head1 DESCRIPTION

Calculates the Fibonacci Number for C<n> (defaults to 28 if
unspecified).

=cut

use strict;
use warnings;

sub fib {
    my $n = shift;
    return $n if ( $n < 2 );
    return fib( $n - 1 ) + fib( $n - 2 );
}
my $N = shift || 28;

print "fib($N) = ", fib($N), "\n";

=head1 SEE ALSO

F<examples/benchmarks/fib.pir>.

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
