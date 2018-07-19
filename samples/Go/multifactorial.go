package main

import "fmt"

func multiFactorial(n, k int) int {
    r := 1
    for ; n > 1; n -= k {
        r *= n
    }
    return r
}

func main() {
    for k := 1; k <= 5; k++ {
        fmt.Print("degree ", k, ":")
        for n := 1; n <= 10; n++ {
            fmt.Print(" ", multiFactorial(n, k))
        }
        fmt.Println()
    }
}
