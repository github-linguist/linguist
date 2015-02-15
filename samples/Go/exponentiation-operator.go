package main

import (
    "errors"
    "fmt"
)

func expI(b, p int) (int, error) {
    if p < 0 {
        return 0, errors.New("negative power not allowed")
    }
    r := 1
    for i := 1; i <= p; i++ {
        r *= b
    }
    return r, nil
}

func expF(b float32, p int) float32 {
    var neg bool
    if p < 0 {
        neg = true
        p = -p
    }
    r := float32(1)
    for pow := b; p > 0; pow *= pow {
        if p&1 == 1 {
            r *= pow
        }
        p >>= 1
    }
    if neg {
        r = 1 / r
    }
    return r
}

func main() {
    ti := func(b, p int) {
        fmt.Printf("%d^%d: ", b, p)
        e, err := expI(b, p)
        if err != nil {
            fmt.Println(err)
        } else {
            fmt.Println(e)
        }
    }

    fmt.Println("expI tests")
    ti(2, 10)
    ti(2, -10)
    ti(-2, 10)
    ti(-2, 11)
    ti(11, 0)

    fmt.Println("overflow undetected")
    ti(10, 10)

    tf := func(b float32, p int) {
        fmt.Printf("%g^%d: %g\n", b, p, expF(b, p))
    }

    fmt.Println("\nexpF tests:")
    tf(2, 10)
    tf(2, -10)
    tf(-2, 10)
    tf(-2, 11)
    tf(11, 0)

    fmt.Println("disallowed in expI, allowed here")
    tf(0, -1)

    fmt.Println("other interesting cases for 32 bit float type")
    tf(10, 39)
    tf(10, -39)
    tf(-10, 39)
}
