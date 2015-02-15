package main

import (
    "fmt"
    "math"
)

func f(x float64) float64 {
    return x*x*x - 3*x*x + 2*x
}

var (
    lower float64 = -.5
    upper float64 = 2.6
    step float64 = 1
)

func main() {
    for x0, x1 := lower, lower+step; x0 < upper; x0, x1 = x1, x1+step {
        if x1 > upper {
            x1 = upper
        }
        r, status := secant(x0, x1)
        if status != "" && r >= x0 && r < x1 {
            fmt.Printf("  %6.3f %s\n", r, status)
        }
    }
}

func secant(x0, x1 float64) (float64, string) {
    var f0 float64
    f1 := f(x0)
    for i := 0; i < 100; i++ {
        f0, f1 = f1, f(x1)
        switch {
        case f1 == 0:
            return x1, "exact"
        case math.Abs(x1-x0) < 1e-6:
            return x1, "approximate"
        }
        x0, x1 = x1, x1-f1*(x1-x0)/(f1-f0)
    }
    return 0, ""
}
