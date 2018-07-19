package main

import (
    "math/rand"
    "time"
)

const mean = 1.0
const stdv = .5

func main() {
    var list [1000]float64
    rand.Seed(time.Now().UnixNano())
    for i := range list {
        list[i] = mean + stdv*rand.NormFloat64()
    }
}
