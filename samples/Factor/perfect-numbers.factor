USING: kernel math math.primes.factors sequences ;
IN: rosettacode.perfect-numbers

: perfect? ( n -- ? )  [ divisors sum ] [ 2 * ] bi = ;
