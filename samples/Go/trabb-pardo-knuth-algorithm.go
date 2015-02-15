package main

import (
    "fmt"
    "log"
    "math"
)

func main() {
    // prompt
    fmt.Print("Enter 11 numbers: ")
    // accept sequence
    var s [11]float64
    for i := 0; i < 11; {
        if _, err := fmt.Scanf("%f", &s[i]); err == nil {
            i++
        }
    }
    // reverse sequence
    for i, item := range s[:5] {
        s[i], s[10-i] = s[10-i], item
    }
    // iterate
    for _, item := range s {
        if result, overflow := f(item); overflow {
            // send alerts to stderr
            log.Printf("f(%g) overflow", item)
        } else {
            // send normal results to stdout
            fmt.Printf("f(%g) = %g\n", item, result)
        }
    }
}

func f(x float64) (float64, bool) {
    result := math.Pow(math.Abs(x), .5) + 5*x*x*x
    return result, result > 400
}
