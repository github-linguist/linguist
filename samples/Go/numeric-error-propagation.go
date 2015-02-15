package main

import (
    "fmt"
    "math"
)

// "uncertain number type"
// a little optimization is to represent the error term with its square.
// this saves some taking of square roots in various places.
type unc struct {
    n float64 // the number
    s float64 // *square* of one sigma error term
}

// constructor, nice to have so it can handle squaring of error term
func newUnc(n, s float64) *unc {
    return &unc{n, s * s}
}

// error term accessor method, nice to have so it can handle recovering
// (non-squared) error term from internal (squared) representation
func (z *unc) errorTerm() float64 {
    return math.Sqrt(z.s)
}

// Arithmetic methods are modeled on the Go big number package.
// The basic scheme is to pass all operands as method arguments, compute
// the result into the method receiver, and then return the receiver as
// the result of the method.  This has an advantage of letting the programer
// determine allocation and use of temporary objects, reducing garbage;
// and has the convenience and efficiency of allowing operations to be chained.

// addition/subtraction
func (z *unc) addC(a *unc, c float64) *unc {
    *z = *a
    z.n += c
    return z
}

func (z *unc) subC(a *unc, c float64) *unc {
    *z = *a
    z.n -= c
    return z
}

func (z *unc) addU(a, b *unc) *unc {
    z.n = a.n + b.n
    z.s = a.s + b.s
    return z
}
func (z *unc) subU(a, b *unc) *unc {
    z.n = a.n - b.n
    z.s = a.s + b.s
    return z
}

// multiplication/division
func (z *unc) mulC(a *unc, c float64) *unc {
    z.n = a.n * c
    z.s = a.s * c * c
    return z
}

func (z *unc) divC(a *unc, c float64) *unc {
    z.n = a.n / c
    z.s = a.s / (c * c)
    return z
}

func (z *unc) mulU(a, b *unc) *unc {
    prod := a.n * b.n
    z.n, z.s = prod, prod*prod*(a.s/(a.n*a.n)+b.s/(b.n*b.n))
    return z
}

func (z *unc) divU(a, b *unc) *unc {
    quot := a.n / b.n
    z.n, z.s = quot, quot*quot*(a.s/(a.n*a.n)+b.s/(b.n*b.n))
    return z
}

// exponentiation
func (z *unc) expC(a *unc, c float64) *unc {
    f := math.Pow(a.n, c)
    g := f * c / a.n
    z.n = f
    z.s = a.s * g * g
    return z
}

func main() {
    x1 := newUnc(100, 1.1)
    x2 := newUnc(200, 2.2)
    y1 := newUnc(50, 1.2)
    y2 := newUnc(100, 2.3)
    var d, d2 unc
    d.expC(d.addU(d.expC(d.subU(x1, x2), 2), d2.expC(d2.subU(y1, y2), 2)), .5)
    fmt.Println("d:    ", d.n)
    fmt.Println("error:", d.errorTerm())
}
