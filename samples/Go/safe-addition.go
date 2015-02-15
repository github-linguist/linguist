package main

import (
    "fmt"
    "math"
)

// type requested by task
type interval struct {
    lower, upper float64
}

// a constructor
func stepAway(x float64) interval {
    return interval {
        math.Nextafter(x, math.Inf(-1)),
        math.Nextafter(x, math.Inf(1))}
}

// function requested by task
func safeAdd(a, b float64) interval {
    return stepAway(a + b)

}

// example
func main() {
    a, b := 1.2, .03
    fmt.Println(a, b, safeAdd(a, b))
}
