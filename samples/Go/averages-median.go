package main

import (
    "fmt"
    "sort"
)

func main() {
    fmt.Println(median([]float64{3, 1, 4, 1}))    // prints 2
    fmt.Println(median([]float64{3, 1, 4, 1, 5})) // prints 3
}

func median(a []float64) float64 {
    sort.Float64s(a)
    half := len(a) / 2
    m := a[half]
    if len(a)%2 == 0 {
        m = (m + a[half-1]) / 2
    }
    return m
}
