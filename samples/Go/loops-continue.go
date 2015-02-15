package main

import "fmt"

func main() {
    for i := 1; i <= 10; i++ {
        fmt.Printf("%d", i)
        if i%5 == 0 {
            fmt.Printf("\n")
            continue
        }
        fmt.Printf(", ")
    }
}
