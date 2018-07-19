package main

import (
    "fmt"
    "math"
)

func main() {
    sum, sumr, prod := 0., 0., 1.
    for n := 1.; n <= 10; n++ {
        sum += n
        sumr += 1 / n
        prod *= n
    }
    a, g, h := sum/10, math.Pow(prod, .1), 10/sumr
    fmt.Println("A:", a, "G:", g, "H:", h)
    fmt.Println("A >= G >= H:", a >= g && g >= h)
}
