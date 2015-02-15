package main

import "log"

func main() {
    // max uint32 is not prime
    c := uint32(1<<32 - 1)
    // a few primes near the top of the range.  source: prime pages.
    for _, p := range []uint32{1<<32 - 5, 1<<32 - 17, 1<<32 - 65, 1<<32 - 99} {
        for ; c > p; c-- {
            if prime(c) {
                log.Fatalf("prime(%d) returned true", c)
            }
        }
        if !prime(p) {
            log.Fatalf("prime(%d) returned false", p)
        }
        c--
    }
}

func prime(n uint32) bool {
    // bases of 2, 7, 61 are sufficient to cover 2^32
    switch n {
    case 0, 1:
        return false
    case 2, 7, 61:
        return true
    }
    // compute s, d where 2^s * d = n-1
    nm1 := n - 1
    d := nm1
    s := 0
    for d&1 == 0 {
        d >>= 1
        s++
    }
    n64 := uint64(n)
    for _, a := range []uint32{2, 7, 61} {
        // compute x := a^d % n
        x := uint64(1)
        p := uint64(a)
        for dr := d; dr > 0; dr >>= 1 {
            if dr&1 != 0 {
                x = x * p % n64
            }
            p = p * p % n64
        }
        if x == 1 || uint32(x) == nm1 {
            continue
        }
        for r := 1; ; r++ {
            if r >= s {
                return false
            }
            x = x * x % n64
            if x == 1 {
                return false
            }
            if uint32(x) == nm1 {
                break
            }
        }
    }
    return true
}
