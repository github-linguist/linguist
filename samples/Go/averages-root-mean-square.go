package main

import (
    "fmt"
    "math"
)

func main() {
    const n = 10
    sum := 0.
    for x := 1.; x <= n; x++ {
        sum += x * x
    }
    fmt.Println(math.Sqrt(sum / n))
}
