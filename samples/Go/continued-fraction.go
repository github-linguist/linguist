package main

import "fmt"

type cfTerm struct {
    a, b int
}

// follows subscript convention of mathworld and WP where there is no b(0).
// cf[0].b is unused in this representation.
type cf []cfTerm

func cfSqrt2(nTerms int) cf {
    f := make(cf, nTerms)
    for n := range f {
        f[n] = cfTerm{2, 1}
    }
    f[0].a = 1
    return f
}

func cfNap(nTerms int) cf {
    f := make(cf, nTerms)
    for n := range f {
        f[n] = cfTerm{n, n - 1}
    }
    f[0].a = 2
    f[1].b = 1
    return f
}

func cfPi(nTerms int) cf {
    f := make(cf, nTerms)
    for n := range f {
        g := 2*n - 1
        f[n] = cfTerm{6, g * g}
    }
    f[0].a = 3
    return f
}

func (f cf) real() (r float64) {
    for n := len(f) - 1; n > 0; n-- {
        r = float64(f[n].b) / (float64(f[n].a) + r)
    }
    return r + float64(f[0].a)
}

func main() {
    fmt.Println("sqrt2:", cfSqrt2(20).real())
    fmt.Println("nap:  ", cfNap(20).real())
    fmt.Println("pi:   ", cfPi(20).real())
}
