package main

import (
    "fmt"
    "math"
)

// specification for an integration
type spec struct {
    lower, upper float64               // bounds for integration
    n            int                   // number of parts
    exact        float64               // expected answer
    fs           string                // mathematical description of function
    f            func(float64) float64 // function to integrate
}

// test cases per task description
var data = []spec{
    spec{0, 1, 100, .25, "x^3", func(x float64) float64 { return x * x * x }},
    spec{1, 100, 1000, float64(math.Log(100)), "1/x",
        func(x float64) float64 { return 1 / x }},
    spec{0, 5000, 5e5, 12.5e6, "x", func(x float64) float64 { return x }},
    spec{0, 6000, 6e6, 18e6, "x", func(x float64) float64 { return x }},
}

// object for associating a printable function name with an integration method
type method struct {
    name      string
    integrate func(spec) float64
}

// integration methods implemented per task description
var methods = []method{
    method{"Rectangular (left)    ", rectLeft},
    method{"Rectangular (right)   ", rectRight},
    method{"Rectangular (midpoint)", rectMid},
    method{"Trapezium             ", trap},
    method{"Simpson's             ", simpson},
}

func rectLeft(t spec) float64 {
    parts := make([]float64, t.n)
    r := t.upper - t.lower
    nf := float64(t.n)
    x0 := t.lower
    for i := range parts {
        x1 := t.lower + float64(i+1)*r/nf
        // x1-x0 better than r/nf.
        // (with r/nf, the represenation error accumulates)
        parts[i] = t.f(x0) * (x1 - x0)
        x0 = x1
    }
    return sum(parts)
}

func rectRight(t spec) float64 {
    parts := make([]float64, t.n)
    r := t.upper - t.lower
    nf := float64(t.n)
    x0 := t.lower
    for i := range parts {
        x1 := t.lower + float64(i+1)*r/nf
        parts[i] = t.f(x1) * (x1 - x0)
        x0 = x1
    }
    return sum(parts)
}

func rectMid(t spec) float64 {
    parts := make([]float64, t.n)
    r := t.upper - t.lower
    nf := float64(t.n)
    // there's a tiny gloss in the x1-x0 trick here.  the correct way
    // would be to compute x's at division boundaries, but we don't need
    // those x's for anything else.  (the function is evaluated on x's
    // at division midpoints rather than division boundaries.)  so, we
    // reuse the midpoint x's, knowing that they will average out just
    // as well.  we just need one extra point, so we use lower-.5.
    x0 := t.lower - .5*r/nf
    for i := range parts {
        x1 := t.lower + (float64(i)+.5)*r/nf
        parts[i] = t.f(x1) * (x1 - x0)
        x0 = x1
    }
    return sum(parts)
}

func trap(t spec) float64 {
    parts := make([]float64, t.n)
    r := t.upper - t.lower
    nf := float64(t.n)
    x0 := t.lower
    f0 := t.f(x0)
    for i := range parts {
        x1 := t.lower + float64(i+1)*r/nf
        f1 := t.f(x1)
        parts[i] = (f0 + f1) * .5 * (x1 - x0)
        x0, f0 = x1, f1
    }
    return sum(parts)
}

func simpson(t spec) float64 {
    parts := make([]float64, 2*t.n+1)
    r := t.upper - t.lower
    nf := float64(t.n)
    // similar to the rectangle midpoint logic explained above,
    // we play a little loose with the values used for dx and dx0.
    dx0 := r / nf
    parts[0] = t.f(t.lower) * dx0
    parts[1] = t.f(t.lower+dx0*.5) * dx0 * 4
    x0 := t.lower + dx0
    for i := 1; i < t.n; i++ {
        x1 := t.lower + float64(i+1)*r/nf
        xmid := (x0 + x1) * .5
        dx := x1 - x0
        parts[2*i] = t.f(x0) * dx * 2
        parts[2*i+1] = t.f(xmid) * dx * 4
        x0 = x1
    }
    parts[2*t.n] = t.f(t.upper) * dx0
    return sum(parts) / 6
}

// sum a list of numbers avoiding loss of precision
func sum(v []float64) float64 {
    if len(v) == 0 {
        return 0
    }
    var parts []float64
    for _, x := range v {
        var i int
        for _, p := range parts {
            sum := p + x
            var err float64
            if math.Abs(x) < math.Abs(p) {
                err = x - (sum - p)
            } else {
                err = p - (sum - x)
            }
            if err != 0 {
                parts[i] = err
                i++
            }
            x = sum
        }
        parts = append(parts[:i], x)
    }
    var sum float64
    for _, x := range parts {
        sum += x
    }
    return sum
}

func main() {
    for _, t := range data {
        fmt.Println("Test case: f(x) =", t.fs)
        fmt.Println("Integration from", t.lower, "to", t.upper,
            "in", t.n, "parts")
        fmt.Printf("Exact result            %.7e     Error\n", t.exact)
        for _, m := range methods {
            a := m.integrate(t)
            e := a - t.exact
            if e < 0 {
                e = -e
            }
            fmt.Printf("%s  %.7e  %.7e\n", m.name, a, e)
        }
        fmt.Println("")
    }
}
