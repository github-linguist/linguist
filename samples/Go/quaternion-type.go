package main

import (
    "fmt"
    "math"
)

type qtn struct {
    r, i, j, k float64
}

var (
    q  = &qtn{1, 2, 3, 4}
    q1 = &qtn{2, 3, 4, 5}
    q2 = &qtn{3, 4, 5, 6}

    r  float64 = 7
)

func main() {
    fmt.Println("Inputs")
    fmt.Println("q:", q)
    fmt.Println("q1:", q1)
    fmt.Println("q2:", q2)
    fmt.Println("r:", r)

    var qr qtn
    fmt.Println("\nFunctions")
    fmt.Println("q.norm():", q.norm())
    fmt.Println("neg(q):", qr.neg(q))
    fmt.Println("conj(q):", qr.conj(q))
    fmt.Println("addF(q, r):", qr.addF(q, r))
    fmt.Println("addQ(q1, q2):", qr.addQ(q1, q2))
    fmt.Println("mulF(q, r):", qr.mulF(q, r))
    fmt.Println("mulQ(q1, q2):", qr.mulQ(q1, q2))
    fmt.Println("mulQ(q2, q1):", qr.mulQ(q2, q1))
}

func (q *qtn) String() string {
    return fmt.Sprintf("(%g, %g, %g, %g)", q.r, q.i, q.j, q.k)
}

func (q *qtn) norm() float64 {
    return math.Sqrt(q.r*q.r + q.i*q.i + q.j*q.j + q.k*q.k)
}

func (z *qtn) neg(q *qtn) *qtn {
    z.r, z.i, z.j, z.k = -q.r, -q.i, -q.j, -q.k
    return z
}

func (z *qtn) conj(q *qtn) *qtn {
    z.r, z.i, z.j, z.k = q.r, -q.i, -q.j, -q.k
    return z
}

func (z *qtn) addF(q *qtn, r float64) *qtn {
    z.r, z.i, z.j, z.k = q.r+r, q.i, q.j, q.k
    return z
}

func (z *qtn) addQ(q1, q2 *qtn) *qtn {
    z.r, z.i, z.j, z.k = q1.r+q2.r, q1.i+q2.i, q1.j+q2.j, q1.k+q2.k
    return z
}

func (z *qtn) mulF(q *qtn, r float64) *qtn {
    z.r, z.i, z.j, z.k = q.r*r, q.i*r, q.j*r, q.k*r
    return z
}

func (z *qtn) mulQ(q1, q2 *qtn) *qtn {
    z.r, z.i, z.j, z.k =
        q1.r*q2.r-q1.i*q2.i-q1.j*q2.j-q1.k*q2.k,
        q1.r*q2.i+q1.i*q2.r+q1.j*q2.k-q1.k*q2.j,
        q1.r*q2.j-q1.i*q2.k+q1.j*q2.r+q1.k*q2.i,
        q1.r*q2.k+q1.i*q2.j-q1.j*q2.i+q1.k*q2.r
    return z
}
