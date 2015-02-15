def primes := {
    var primesCache := [2]
    /** A collection of all prime numbers. */
    def primes {
        to iterate(f) {
            primesCache.iterate(f)
            for x in (int > primesCache.last()) {
                if (isPrime(x)) {
                    f(primesCache.size(), x)
                    primesCache with= x
                }
            }
        }
    }
}

def primeDecomposition(var x :(int > 0)) {
    var factors := []
    for p in primes {
        while (x % p <=> 0) {
            factors with= p
            x //= p
        }
        if (x <=> 1) {
            break
        }
    }
    return factors
}
