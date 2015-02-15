package main

import (
    "fmt"
    "math"
)

const ε = 1e-14

func agm(a, g float64) float64 {
    for math.Abs(a-g) > math.Abs(a)*ε {
        a, g = (a+g)*.5, math.Sqrt(a*g)
    }
    return a
}

func main() {
    fmt.Println(agm(1, 1/math.Sqrt2))
}
