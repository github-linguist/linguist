package main

import (
    "fmt"
    "math"
)

func remarkable(n int) int {
    return n + int(.5+math.Sqrt(float64(n)))
}

func main() {
    // task 1
    fmt.Println("  n  r(n)")
    fmt.Println("---  ---")
    for n := 1; n <= 22; n++ {
        fmt.Printf("%3d  %3d\n", n, remarkable(n))
    }

    // task 2
    const limit = 1e6
    fmt.Println("\nChecking for squares for n <", limit)
    next := 2
    nextSq := 4
    for n := 1; n < limit; n++ {
        r := remarkable(n)
        switch {
        case r == nextSq:
            panic(n)
        case r > nextSq:
            fmt.Println(nextSq, "didn't occur")
            next++
            nextSq = next * next
        }
    }
    fmt.Println("No squares occur for n <", limit)
}
