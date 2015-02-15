package main

import "fmt"

func main() {
    n := []float64{-42, 0, -12, 1}
    d := []float64{-3, 1}
    fmt.Println("N:", n)
    fmt.Println("D:", d)
    q, r, ok := pld(n, d)
    if ok {
        fmt.Println("Q:", q)
        fmt.Println("R:", r)
    } else {
        fmt.Println("error")
    }
}

func degree(p []float64) int {
    for d := len(p) - 1; d >= 0; d-- {
        if p[d] != 0 {
            return d
        }
    }
    return -1
}

func pld(nn, dd []float64) (q, r []float64, ok bool) {
    if degree(dd) < 0 {
        return
    }
    nn = append(r, nn...)
    if degree(nn) >= degree(dd) {
        q = make([]float64, degree(nn)-degree(dd)+1)
        for degree(nn) >= degree(dd) {
            d := make([]float64, degree(nn)+1)
            copy(d[degree(nn)-degree(dd):], dd)
            q[degree(nn)-degree(dd)] = nn[degree(nn)] / d[degree(d)]
            for i := range d {
                d[i] *= q[degree(nn)-degree(dd)]
                nn[i] -= d[i]
            }
        }
    }
    return q, nn, true
}
