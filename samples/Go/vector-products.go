package main

import "fmt"

type vector struct {
    x, y, z float64
}

var (
    a = vector{3, 4, 5}
    b = vector{4, 3, 5}
    c = vector{-5, -12, -13}
)

func dot(a, b vector) float64 {
    return a.x*b.x + a.y*b.y + a.z*b.z
}

func cross(a, b vector) vector {
    return vector{a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x}
}

func s3(a, b, c vector) float64 {
    return dot(a, cross(b, c))
}

func v3(a, b, c vector) vector {
    return cross(a, cross(b, c))
}

func main() {
    fmt.Println(dot(a, b))
    fmt.Println(cross(a, b))
    fmt.Println(s3(a, b, c))
    fmt.Println(v3(a, b, c))
}
