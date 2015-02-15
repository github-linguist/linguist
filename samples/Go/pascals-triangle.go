package main

import "fmt"

func printTriangle(n int) {
    // degenerate cases
    if n <= 0 {
        return
    }
    fmt.Println(1)
    if n == 1 {
        return
    }
    // iterate over rows, zero based
    a := make([]int, (n+1)/2)
    a[0] = 1
    for row, middle := 1, 0; row < n; row++ {
        // generate new row
        even := row&1 == 0
        if even {
            a[middle+1] = a[middle] * 2
        }
        for i := middle; i > 0; i-- {
            a[i] += a[i-1]
        }
        // print row
        for i := 0; i <= middle; i++ {
            fmt.Print(a[i], " ")
        }
        if even {
            middle++
        }
        for i := middle; i >= 0; i-- {
            fmt.Print(a[i], " ")
        }
        fmt.Println("")
    }
}

func main() {
    printTriangle(4)
}
