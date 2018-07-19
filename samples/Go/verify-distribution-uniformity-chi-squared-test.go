package main

import (
    "fmt"
    "math"
)

type ifctn func(float64) float64

func simpson38(f ifctn, a, b float64, n int) float64 {
    h := (b - a) / float64(n)
    h1 := h / 3
    sum := f(a) + f(b)
    for j := 3*n - 1; j > 0; j-- {
        if j%3 == 0 {
            sum += 2 * f(a+h1*float64(j))
        } else {
            sum += 3 * f(a+h1*float64(j))
        }
    }
    return h * sum / 8
}

func gammaIncQ(a, x float64) float64 {
    aa1 := a - 1
    var f ifctn = func(t float64) float64 {
        return math.Pow(t, aa1) * math.Exp(-t)
    }
    y := aa1
    h := 1.5e-2
    for f(y)*(x-y) > 2e-8 && y < x {
        y += .4
    }
    if y > x {
        y = x
    }
    return 1 - simpson38(f, 0, y, int(y/h/math.Gamma(a)))
}

func chi2ud(ds []int) float64 {
    var sum, expected float64
    for _, d := range ds {
        expected += float64(d)
    }
    expected /= float64(len(ds))
    for _, d := range ds {
        x := float64(d) - expected
        sum += x * x
    }
    return sum / expected
}

func chi2p(dof int, distance float64) float64 {
    return gammaIncQ(.5*float64(dof), .5*distance)
}

const sigLevel = .05

func main() {
    for _, dset := range [][]int{
        {199809, 200665, 199607, 200270, 199649},
        {522573, 244456, 139979, 71531, 21461},
    } {
        utest(dset)
    }
}

func utest(dset []int) {
    fmt.Println("Uniform distribution test")
    var sum int
    for _, c := range dset {
        sum += c
    }
    fmt.Println(" dataset:", dset)
    fmt.Println(" samples:                      ", sum)
    fmt.Println(" categories:                   ", len(dset))

    dof := len(dset) - 1
    fmt.Println(" degrees of freedom:           ", dof)

    dist := chi2ud(dset)
    fmt.Println(" chi square test statistic:    ", dist)

    p := chi2p(dof, dist)
    fmt.Println(" p-value of test statistic:    ", p)

    sig := p < sigLevel
    fmt.Printf(" significant at %2.0f%% level?      %t\n", sigLevel*100, sig)
    fmt.Println(" uniform?                      ", !sig, "\n")
}
