package main

import "math"
import "fmt"

// user-defined function, per task.  Other math functions used are built-in.
func cube(x float64) float64 { return math.Pow(x, 3) }

// ffType and compose function taken from Function composition task
type ffType func(float64) float64

func compose(f, g ffType) ffType {
    return func(x float64) float64 {
        return f(g(x))
    }
}

func main() {
    // collection A
    funclist := []ffType{math.Sin, math.Cos, cube}
    // collection B
    funclisti := []ffType{math.Asin, math.Acos, math.Cbrt}
    for i := 0; i < 3; i++ {
        // apply composition and show result
        fmt.Println(compose(funclisti[i], funclist[i])(.5))
    }
}
