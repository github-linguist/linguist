package main

import (
    "fmt"
    "math/big"
)

var primes = []uint{3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
    53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127}

var mersennes = []uint{521, 607, 1279, 2203, 2281, 3217, 4253, 4423, 9689,
    9941, 11213, 19937, 21701, 23209, 44497, 86243, 110503, 132049, 216091,
    756839, 859433, 1257787, 1398269, 2976221, 3021377, 6972593, 13466917,
    20996011, 24036583}

func main() {
    llTest(primes)
    fmt.Println()
    llTest(mersennes)
}

func llTest(ps []uint) {
    var s, m big.Int
    one := big.NewInt(1)
    two := big.NewInt(2)
    for _, p := range ps {
        m.Sub(m.Lsh(one, p), one)
        s.SetInt64(4)
        for i := uint(2); i < p; i++ {
            s.Mod(s.Sub(s.Mul(&s, &s), two), &m)
        }
        if s.BitLen() == 0 {
            fmt.Printf("M%d ", p)
        }
    }
}
