package main

import "fmt"

func v2(n uint) (r float64) {
    p := .5
    for n > 0 {
        if n&1 == 1 {
            r += p
        }
        p *= .5
        n >>= 1
    }
    return
}

func newV(base uint) func(uint) float64 {
    invb := 1 / float64(base)
    return func(n uint) (r float64) {
        p := invb
        for n > 0 {
            r += p * float64(n%base)
            p *= invb
            n /= base
        }
        return
    }
}

func main() {
    fmt.Println("Base 2:")
    for i := uint(0); i < 10; i++ {
        fmt.Println(i, v2(i))
    }
    fmt.Println("Base 3:")
    v3 := newV(3)
    for i := uint(0); i < 10; i++ {
        fmt.Println(i, v3(i))
    }
}
