package main

import (
    "fmt"
    "math"
)

func max(a, b uint64) uint64 {
    if a > b {
        return a
    }
    return b
}

func min(a, b uint64) uint64 {
    if a < b {
        return a
    }
    return b
}

func ndigits(x uint64) (n int) {
    for ; x > 0; x /= 10 {
        n++
    }
    return
}

func dtally(x uint64) (t uint64) {
    for ; x > 0; x /= 10 {
        t += 1 << (x % 10 * 6)
    }
    return
}

var tens [20]uint64

func init() {
    tens[0] = 1
    for i := 1; i < 20; i++ {
        tens[i] = tens[i-1] * 10
    }
}

func fangs(x uint64) (f []uint64) {
    nd := ndigits(x)
    if nd&1 == 1 {
        return
    }
    nd /= 2
    lo := max(tens[nd-1], (x+tens[nd]-2)/(tens[nd]-1))
    hi := min(x/lo, uint64(math.Sqrt(float64(x))))
    t := dtally(x)
    for a := lo; a <= hi; a++ {
        b := x / a
        if a*b == x &&
            (a%10 > 0 || b%10 > 0) &&
            t == dtally(a)+dtally(b) {
            f = append(f, a)
        }
    }
    return
}

func showFangs(x uint64, f []uint64) {
    fmt.Print(x)
    if len(f) > 1 {
        fmt.Println()
    }
    for _, a := range f {
        fmt.Println(" =", a, "×", x/a)
    }
}

func main() {
    for x, n := uint64(1), 0; n < 26; x++ {
        if f := fangs(x); len(f) > 0 {
            n++
            fmt.Printf("%2d: ", n)
            showFangs(x, f)
        }
    }
    fmt.Println()
    for _, x := range []uint64{16758243290880, 24959017348650, 14593825548650} {
        if f := fangs(x); len(f) > 0 {
            showFangs(x, f)
        } else {
            fmt.Println(x, "is not vampiric")
        }
    }
}
