package main

import (
    "fmt"
    "math"
)

const d = 30.
const r = d * math.Pi / 180

var s = .5
var c = math.Sqrt(3) / 2
var t = 1 / math.Sqrt(3)

func main() {
    fmt.Printf("sin(%9.6f deg) = %f\n", d, math.Sin(d*math.Pi/180))
    fmt.Printf("sin(%9.6f rad) = %f\n", r, math.Sin(r))
    fmt.Printf("cos(%9.6f deg) = %f\n", d, math.Cos(d*math.Pi/180))
    fmt.Printf("cos(%9.6f rad) = %f\n", r, math.Cos(r))
    fmt.Printf("tan(%9.6f deg) = %f\n", d, math.Tan(d*math.Pi/180))
    fmt.Printf("tan(%9.6f rad) = %f\n", r, math.Tan(r))
    fmt.Printf("asin(%f) = %9.6f deg\n", s, math.Asin(s)*180/math.Pi)
    fmt.Printf("asin(%f) = %9.6f rad\n", s, math.Asin(s))
    fmt.Printf("acos(%f) = %9.6f deg\n", c, math.Acos(c)*180/math.Pi)
    fmt.Printf("acos(%f) = %9.6f rad\n", c, math.Acos(c))
    fmt.Printf("atan(%f) = %9.6f deg\n", t, math.Atan(t)*180/math.Pi)
    fmt.Printf("atan(%f) = %9.6f rad\n", t, math.Atan(t))
}
