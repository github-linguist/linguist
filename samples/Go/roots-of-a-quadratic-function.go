package main

import (
    "fmt"
    "math"
)

func qr(a, b, c float64) ([]float64, []complex128) {
    d := b*b-4*a*c
    switch {
    case d == 0:
        // single root
        return []float64{-b/(2*a)}, nil
    case d > 0:
        // two real roots
        if b < 0 {
            d = math.Sqrt(d)-b
        } else {
            d = -math.Sqrt(d)-b
        }
        return []float64{d/(2*a), (2*c)/d}, nil
    case d < 0:
        // two complex roots

        den := 1/(2*a)
        t1 := complex(-b*den, 0)
        t2 := complex(0, math.Sqrt(-d)*den)
        return nil, []complex128{t1+t2, t1-t2}
    }
    // otherwise d overflowed or a coefficient was NAN
    return []float64{d}, nil
}

func test(a, b, c float64) {
    fmt.Print("coefficients: ", a, b, c, " -> ")
    r, i := qr(a, b, c)
    switch len(r) {
    case 1:
        fmt.Println("one real root:", r[0])
    case 2:
        fmt.Println("two real roots:", r[0], r[1])
    default:
        fmt.Println("two complex roots:", i[0], i[1])
    }
}

func main() {
    for _, c := range [][3]float64{
        {1, -2, 1},
        {1, 0, 1},
        {1, -10, 1},
        {1, -1000, 1},
        {1, -1e9, 1},
    } {
        test(c[0], c[1], c[2])
    }
}
