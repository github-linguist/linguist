package main

import "fmt"

func main() {
    for i := 0; i <= 20; i++ {
        fmt.Printf("%2d %7b\n", i, zeckendorf(i))
    }
}

func zeckendorf(n int) int {
    // initial arguments of fib0 = 1 and fib1 = 1 will produce
    // the Fibonacci sequence {1, 2, 3,..} on the stack as successive
    // values of fib1.
    _, set := zr(1, 1, n, 0)
    return set
}

func zr(fib0, fib1, n int, bit uint) (remaining, set int) {
    if fib1 > n {
        return n, 0
    }
    // recurse.
    // construct sequence on the way in, construct ZR on the way out.
    remaining, set = zr(fib1, fib0+fib1, n, bit+1)
    if fib1 <= remaining {
        set |= 1 << bit
        remaining -= fib1
    }
    return
}
