package main

import (
    "fmt"
    "strconv"
)

func zz(n int) []int {
    r := make([]int, n*n)
    i := 0
    n2 := n * 2
    for d := 1; d <= n2; d++ {
        x := d - n
        if x < 0 {
            x = 0
        }
        y := d - 1
        if y > n-1 {
            y = n - 1
        }
        j := n2 - d
        if j > d {
            j = d
        }
        for k := 0; k < j; k++ {
            if d&1 == 0 {
                r[(x+k)*n+y-k] = i
            } else {
                r[(y-k)*n+x+k] = i
            }
            i++
        }
    }

    return r
}

func main() {
    const n = 5
    w := len(strconv.Itoa(n*n - 1))
    for i, e := range zz(n) {
        fmt.Printf("%*d ", w, e)
        if i%n == n-1 {
            fmt.Println("")
        }
    }
}
