package main

import (
    "fmt"
    "math"
)

func newRsdv() func(float64) float64 {
    var n, a, q  float64
    return func(x float64) float64 {
        n++
        a1 := a+(x-a)/n
        q, a = q+(x-a)*(x-a1), a1
        return math.Sqrt(q/n)
    }
}

func main() {
    r := newRsdv()
    for _, x := range []float64{2,4,4,4,5,5,7,9} {
        fmt.Println(r(x))
    }
}
