package main

import (
    "fmt"
    "strconv"
)

var n = 5

func main() {
    if n < 1 {
        return
    }
    top, left, bottom, right := 0, 0, n-1, n-1
    sz := n * n
    a := make([]int, sz)
    i := 0
    for left < right {
        // work right, along top
        for c := left; c <= right; c++ {
            a[top*n+c] = i
            i++
        }
        top++
        // work down right side
        for r := top; r <= bottom; r++ {
            a[r*n+right] = i
            i++
        }
        right--
        if top == bottom {
            break
        }
        // work left, along bottom
        for c := right; c >= left; c-- {
            a[bottom*n+c] = i
            i++
        }
        bottom--
        // work up left side
        for r := bottom; r >= top; r-- {
            a[r*n+left] = i
            i++
        }
        left++
    }
    // center (last) element
    a[top*n+left] = i

    // print
    w := len(strconv.Itoa(n*n - 1))
    for i, e := range a {
        fmt.Printf("%*d ", w, e)
        if i%n == n-1 {
            fmt.Println("")
        }
    }
}
