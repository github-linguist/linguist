package main

import (
    "fmt"
    "math"
)

func main() {
    // task 1: build 32 row trig table
    const nn = 32
    const step = .05
    xVal := make([]float64, nn)
    tSin := make([]float64, nn)
    tCos := make([]float64, nn)
    tTan := make([]float64, nn)
    for i := range xVal {
        xVal[i] = float64(i) * step
        tSin[i], tCos[i] = math.Sincos(xVal[i])
        tTan[i] = tSin[i] / tCos[i]
    }
    // task 2: define inverses
    iSin := thieleInterpolator(tSin, xVal)
    iCos := thieleInterpolator(tCos, xVal)
    iTan := thieleInterpolator(tTan, xVal)
    // task 3: demonstrate identities
    fmt.Printf("%16.14f\n", 6*iSin(.5))
    fmt.Printf("%16.14f\n", 3*iCos(.5))
    fmt.Printf("%16.14f\n", 4*iTan(1))
}

func thieleInterpolator(x, y []float64) func(float64) float64 {
    n := len(x)
    ρ := make([][]float64, n)
    for i := range ρ {
        ρ[i] = make([]float64, n-i)
        ρ[i][0] = y[i]
    }
    for i := 0; i < n-1; i++ {
        ρ[i][1] = (x[i] - x[i+1]) / (ρ[i][0] - ρ[i+1][0])
    }
    for i := 2; i < n; i++ {
        for j := 0; j < n-i; j++ {
            ρ[j][i] = (x[j]-x[j+i])/(ρ[j][i-1]-ρ[j+1][i-1]) + ρ[j+1][i-2]
        }
    }
    // ρ0 used in closure.  the rest of ρ becomes garbage.
    ρ0 := ρ[0]
    return func(xin float64) float64 {
        var a float64
        for i := n - 1; i > 1; i-- {
            a = (xin - x[i-1]) / (ρ0[i] - ρ0[i-2] + a)
        }
        return y[0] + (xin-x[0])/(ρ0[1]+a)
    }
}
