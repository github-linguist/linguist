package main

import (
    "fmt"
    "math"
)

type circle struct {
    x, y, r float64
}

func main() {
    c1 := circle{0, 0, 1}
    c2 := circle{4, 0, 1}
    c3 := circle{2, 4, 2}
    fmt.Println(ap(c1, c2, c3, true))
    fmt.Println(ap(c1, c2, c3, false))
}

func ap(c1, c2, c3 circle, s bool) circle {
    x1sq := c1.x * c1.x
    y1sq := c1.y * c1.y
    r1sq := c1.r * c1.r
    x2sq := c2.x * c2.x
    y2sq := c2.y * c2.y
    r2sq := c2.r * c2.r
    x3sq := c3.x * c3.x
    y3sq := c3.y * c3.y
    r3sq := c3.r * c3.r
    v11 := 2 * (c2.x - c1.x)
    v12 := 2 * (c2.y - c1.y)
    v13 := x1sq - x2sq + y1sq - y2sq - r1sq + r2sq
    v14 := 2 * (c2.r - c1.r)
    v21 := 2 * (c3.x - c2.x)
    v22 := 2 * (c3.y - c2.y)
    v23 := x2sq - x3sq + y2sq - y3sq - r2sq + r3sq
    v24 := 2 * (c3.r - c2.r)
    if s {
        v14 = -v14
        v24 = -v24
    }
    w12 := v12 / v11
    w13 := v13 / v11
    w14 := v14 / v11
    w22 := v22/v21 - w12
    w23 := v23/v21 - w13
    w24 := v24/v21 - w14
    p := -w23 / w22
    q := w24 / w22
    m := -w12*p - w13
    n := w14 - w12*q
    a := n*n + q*q - 1
    b := m*n - n*c1.x + p*q - q*c1.y
    if s {
        b -= c1.r
    } else {
        b += c1.r
    }
    b *= 2
    c := x1sq + m*m - 2*m*c1.x + p*p + y1sq - 2*p*c1.y - r1sq
    d := b*b - 4*a*c
    rs := (-b - math.Sqrt(d)) / (2 * a)
    return circle{m + n*rs, p + q*rs, rs}
}
