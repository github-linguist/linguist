package main

import "fmt"

type is func() int

func newSum() is {
    var ms is
    ms = func() int {
        ms = newSum()
        return ms()
    }
    var msd, d int
    return func() int {
        if d < 9 {
            d++
        } else {
            d = 0
            msd = ms()
        }
        return msd + d
    }
}

func newHarshard() is {
    i := 0
    sum := newSum()
    return func() int {
        for i++; i%sum() != 0; i++ {
        }
        return i
    }
}

func main() {
    h := newHarshard()
    fmt.Print(h())
    for i := 1; i < 20; i++ {
        fmt.Print(" ", h())
    }
    fmt.Println()
    h = newHarshard()
    n := h()
    for ; n <= 1000; n = h() {
    }
    fmt.Println(n)
}
