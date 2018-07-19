package main

import (
    "fmt"
    "math"
)

func mean(v []float64) (m float64, ok bool) {
    if len(v) == 0 {
        return
    }
    // an algorithm that attempts to retain accuracy
    // with widely different values.
    var parts []float64
    for _, x := range v {
        var i int
        for _, p := range parts {
            sum := p + x
            var err float64
            switch ax, ap := math.Abs(x), math.Abs(p); {
            case ax < ap:
                err = x - (sum - p)
            case ap < ax:
                err = p - (sum - x)
            }
            if err != 0 {
                parts[i] = err
                i++
            }
            x = sum
        }
        parts = append(parts[:i], x)
    }
    var sum float64
    for _, x := range parts {
        sum += x
    }
    return sum / float64(len(v)), true
}

func main() {
    for _, v := range [][]float64{
        []float64{},                         // mean returns ok = false
        []float64{math.Inf(1), math.Inf(1)}, // answer is +Inf

        // answer is NaN, and mean returns ok = true, indicating NaN
        // is the correct result
        []float64{math.Inf(1), math.Inf(-1)},

        []float64{3, 1, 4, 1, 5, 9},

        // large magnitude numbers cancel. answer is mean of small numbers.
        []float64{1e20, 3, 1, 4, 1, 5, 9, -1e20},

        []float64{10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, .11},
        []float64{10, 20, 30, 40, 50, -100, 4.7, -11e2},
    } {
        fmt.Println("Vector:", v)
        if m, ok := mean(v); ok {
            fmt.Printf("Mean of %d numbers is %g\n\n", len(v), m)
        } else {
            fmt.Println("Mean undefined\n")
        }
    }
}
