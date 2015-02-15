package main

import (
    "fmt"
    "math"
)

// limit search to small primes.  really this is higher than
// you'd want it, but it's fun to factor M67.
const qlimit = 2e8

func main() {
    mtest(31)
    mtest(67)
    mtest(929)
}

func mtest(m int32) {
    // the function finds odd prime factors by
    // searching no farther than sqrt(N), where N = 2^m-1.
    // the first odd prime is 3, 3^2 = 9, so M3 = 7 is still too small.
    // M4 = 15 is first number for which test is meaningful.
    if m < 4 {
        fmt.Printf("%d < 4.  M%d not tested.\n", m, m)
        return
    }
    flimit := math.Sqrt(math.Pow(2, float64(m)) - 1)
    var qlast int32
    if flimit < qlimit {
        qlast = int32(flimit)
    } else {
        qlast = qlimit
    }
    composite := make([]bool, qlast+1)
    sq := int32(math.Sqrt(float64(qlast)))
loop:
    for q := int32(3); ; {
        if q <= sq {
            for i := q * q; i <= qlast; i += q {
                composite[i] = true
            }
        }
        if q8 := q % 8; (q8 == 1 || q8 == 7) && modPow(2, m, q) == 1 {
            fmt.Printf("M%d has factor %d\n", m, q)
            return
        }
        for {
            q += 2
            if q > qlast {
                break loop
            }
            if !composite[q] {
                break
            }
        }
    }
    fmt.Printf("No factors of M%d found.\n", m)
}

// base b to power p, mod m
func modPow(b, p, m int32) int32 {
    pow := int64(1)
    b64 := int64(b)
    m64 := int64(m)
    bit := uint(30)
    for 1<<bit&p == 0 {
        bit--
    }
    for {
        pow *= pow
        if 1<<bit&p != 0 {
            pow *= b64
        }
        pow %= m64
        if bit == 0 {
            break
        }
        bit--
    }
    return int32(pow)
}
