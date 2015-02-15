package main

import (
    "fmt"
    "math"
)

// cFunc for continuous function.  A type definition for convenience.
type cFunc func(float64) float64

func main() {
    fmt.Println("integral:", glq(math.Exp, -3, 3, 5))
}

// glq integrates f from a to b by Guass-Legendre quadrature using n nodes.
// For the task, it also shows the intermediate values determining the nodes:
// the n roots of the order n Legendre polynomal and the corresponding n
// weights used for the integration.
func glq(f cFunc, a, b float64, n int) float64 {
    x, w := glqNodes(n, f)
    show := func(label string, vs []float64) {
        fmt.Printf("%8s: ", label)
        for _, v := range vs {
            fmt.Printf("%8.5f ", v)
        }
        fmt.Println()
    }
    show("nodes", x)
    show("weights", w)
    var sum float64
    bma2 := (b - a) * .5
    bpa2 := (b + a) * .5
    for i, xi := range x {
        sum += w[i] * f(bma2*xi+bpa2)
    }
    return bma2 * sum
}

// glqNodes computes both nodes and weights for a Gauss-Legendre
// Quadrature integration.  Parameters are n, the number of nodes
// to compute and f, a continuous function to integrate.  Return
// values have len n.
func glqNodes(n int, f cFunc) (node []float64, weight []float64) {
    p := legendrePoly(n)
    pn := p[n]
    n64 := float64(n)
    dn := func(x float64) float64 {
        return (x*pn(x) - p[n-1](x)) * n64 / (x*x - 1)
    }
    node = make([]float64, n)
    for i := range node {
        x0 := math.Cos(math.Pi * (float64(i+1) - .25) / (n64 + .5))
        node[i] = newtonRaphson(pn, dn, x0)
    }
    weight = make([]float64, n)
    for i, x := range node {
        dnx := dn(x)
        weight[i] = 2 / ((1 - x*x) * dnx * dnx)
    }
    return
}

// legendrePoly constructs functions that implement Lengendre polynomials.
// This is done by function composition by recurrence relation (Bonnet's.)
// For given n, n+1 functions are returned, computing P0 through Pn.
func legendrePoly(n int) []cFunc {
    r := make([]cFunc, n+1)
    r[0] = func(float64) float64 { return 1 }
    r[1] = func(x float64) float64 { return x }
    for i := 2; i <= n; i++ {
        i2m1 := float64(i*2 - 1)
        im1 := float64(i - 1)
        rm1 := r[i-1]
        rm2 := r[i-2]
        invi := 1 / float64(i)
        r[i] = func(x float64) float64 {
            return (i2m1*x*rm1(x) - im1*rm2(x)) * invi
        }
    }
    return r
}

// newtonRaphson is general purpose, although totally primitive, simply
// panicking after a fixed number of iterations without convergence to
// a fixed error.  Parameter f must be a continuous function,
// df its derivative, x0 an initial guess.
func newtonRaphson(f, df cFunc, x0 float64) float64 {
    for i := 0; i < 30; i++ {
        x1 := x0 - f(x0)/df(x0)
        if math.Abs(x1-x0) <= math.Abs(x0*1e-15) {
            return x1
        }
        x0 = x1
    }
    panic("no convergence")
}
