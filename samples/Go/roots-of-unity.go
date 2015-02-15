package main

import (
    "fmt"
    "math"
    "math/cmplx"
)

func main() {
    for n := 2; n <= 5; n++ {
        fmt.Printf("%d roots of 1:\n", n)
        for _, r := range roots(n) {
            fmt.Printf("  %18.15f\n", r)
        }
    }
}

func roots(n int) []complex128 {
    r := make([]complex128, n)
    for i := 0; i < n; i++ {
        r[i] = cmplx.Rect(1, 2*math.Pi*float64(i)/float64(n))
    }
    return r
}
