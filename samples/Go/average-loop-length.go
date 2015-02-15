package main

import (
    "fmt"
    "math"
    "math/rand"
)

const nmax = 20

func main() {
    fmt.Println(" N    average    analytical    (error)")
    fmt.Println("===  =========  ============  =========")
    for n := 1; n <= nmax; n++ {
        a := avg(n)
        b := ana(n)
        fmt.Printf("%3d  %9.4f  %12.4f  (%6.2f%%)\n",
            n, a, b, math.Abs(a-b)/b*100)
    }
}

func avg(n int) float64 {
    const tests = 1e4
    sum := 0
    for t := 0; t < tests; t++ {
        var v [nmax]bool
        for x := 0; !v[x]; x = rand.Intn(n) {
            v[x] = true
            sum++
        }
    }
    return float64(sum) / tests
}

func ana(n int) float64 {
    nn := float64(n)
    term := 1.
    sum := 1.
    for i := nn - 1; i >= 1; i-- {
        term *= i / nn
        sum += term
    }
    return sum
}
