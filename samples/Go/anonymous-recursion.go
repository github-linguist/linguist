package main

import "fmt"

func main() {
    for _, n := range []int{0, 1, 2, 3, 4, 5, 10, 40, -1} {
        f, ok := arFib(n)
        if ok {
            fmt.Printf("fib %d = %d\n", n, f)
        } else {
            fmt.Println("fib undefined for negative numbers")
        }
    }
}

func arFib(n int) (int, bool) {
    switch {
    case n < 0:
        return 0, false
    case n < 2:
        return n, true
    }
    return yc(func(recurse fn) fn {
        return func(left, term1, term2 int) int {
            if left == 0 {
                return term1+term2
            }
            return recurse(left-1, term1+term2, term1)
        }
    })(n-2, 1, 0), true
}

type fn func(int, int, int) int
type ff func(fn) fn
type fx func(fx) fn

func yc(f ff) fn {
    return func(x fx) fn {
        return f(func(a1, a2, a3 int) int {
            return x(x)(a1, a2, a3)
        })
    }(func(x fx) fn {
        return f(func(a1, a2, a3 int) int {
            return x(x)(a1, a2, a3)
        })
    })
}
