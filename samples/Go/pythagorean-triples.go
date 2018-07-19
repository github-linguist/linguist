package main

import "fmt"

var total, prim, maxPeri int64

func newTri(s0, s1, s2 int64) {
    if p := s0 + s1 + s2; p <= maxPeri {
        prim++
        total += maxPeri / p
        newTri(+1*s0-2*s1+2*s2, +2*s0-1*s1+2*s2, +2*s0-2*s1+3*s2)
        newTri(+1*s0+2*s1+2*s2, +2*s0+1*s1+2*s2, +2*s0+2*s1+3*s2)
        newTri(-1*s0+2*s1+2*s2, -2*s0+1*s1+2*s2, -2*s0+2*s1+3*s2)
    }
}

func main() {
    for maxPeri = 100; maxPeri <= 1e11; maxPeri *= 10 {
        prim = 0
        total = 0
        newTri(3, 4, 5)
        fmt.Printf("Up to %d:  %d triples, %d primitives\n",
            maxPeri, total, prim)
    }
}
