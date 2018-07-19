package main

import (
    "fmt"
    "permute"
)

func determinant(m [][]float64) (d float64) {
    p := make([]int, len(m))
    for i := range p {
        p[i] = i
    }
    it := permute.Iter(p)
    for s := it(); s != 0; s = it() {
        pr := 1.
        for i, σ := range p {
            pr *= m[i][σ]
        }
        d += float64(s) * pr
    }
    return
}

func permanent(m [][]float64) (d float64) {
    p := make([]int, len(m))
    for i := range p {
        p[i] = i
    }
    it := permute.Iter(p)
    for s := it(); s != 0; s = it() {
        pr := 1.
        for i, σ := range p {
            pr *= m[i][σ]
        }
        d += pr
    }
    return
}

var m2 = [][]float64{
    {1, 2},
    {3, 4}}

var m3 = [][]float64{
    {2, 9, 4},
    {7, 5, 3},
    {6, 1, 8}}

func main() {
    fmt.Println(determinant(m2), permanent(m2))
    fmt.Println(determinant(m3), permanent(m3))
}
