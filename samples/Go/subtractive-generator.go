package main

import (
    "fmt"
    "os"
)

// A fairly close port of the Bentley code, but parameterized to better
// conform to the algorithm description in the task, which didn't assume
// constants for i, j, m, and seed.  also parameterized here are k,
// the reordering factor, and s, the number of intial numbers to discard,
// as these are dependant on i.
func newSG(i, j, k, s, m, seed int) func() int {
    // check parameters for range and mutual consistency
    assert(i > 0, "i must be > 0")
    assert(j > 0, "j must be > 0")
    assert(i > j, "i must be > j")
    assert(k > 0, "k must be > 0")
    p, q := i, k
    if p < q {
        p, q = q, p
    }
    for q > 0 {
        p, q = q, p%q
    }
    assert(p == 1, "k, i must be relatively prime")
    assert(s >= i, "s must be >= i")
    assert(m > 0, "m must be > 0")
    assert(seed >= 0, "seed must be >= 0")
    // variables for closure f
    arr := make([]int, i)
    a := 0
    b := j
    // f is Bently RNG lprand
    f := func() int {
        if a == 0 {
            a = i
        }
        a--
        if b == 0 {
            b = i
        }
        b--
        t := arr[a] - arr[b]
        if t < 0 {
            t += m
        }
        arr[a] = t
        return t
    }
    // Bentley seed algorithm sprand
    last := seed
    arr[0] = last
    next := 1
    for i0 := 1; i0 < i; i0++ {
        ii := k * i0 % i
        arr[ii] = next
        next = last - next
        if next < 0 {
            next += m
        }
        last = arr[ii]
    }
    for i0 := i; i0 < s; i0++ {
        f()
    }
    // return the fully initialized RNG
    return f
}

func assert(p bool, m string) {
    if !p {
        fmt.Println(m)
        os.Exit(1)
    }
}

func main() {
    // 1st test case included in program_tools/universal.c.
    // (2nd test case fails.  A single digit is missing, indicating a typo.)
    ptTest(0, 1, []int{921674862, 250065336, 377506581})

    // reproduce 3 values given in task description
    skip := 220
    sg := newSG(55, 24, 21, skip, 1e9, 292929)
    for n := skip; n <= 222; n++ {
        fmt.Printf("r(%d) = %d\n", n, sg())
    }
}

func ptTest(nd, s int, rs []int) {
    sg := newSG(55, 24, 21, 220+nd, 1e9, s)
    for _, r := range rs {
        a := sg()
        if r != a {
            fmt.Println("Fail")
            os.Exit(1)
        }
    }
}
