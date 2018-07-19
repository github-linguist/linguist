package main

import "fmt"

func main() {
    fmt.Println("1: 1")
    for i := 2; ; i++ {
        fmt.Printf("%d: ", i)
        var x string
        for n, f := i, 2; n != 1; f++ {
            for m := n % f; m == 0; m = n % f {
                fmt.Print(x, f)
                x = "×"
                n /= f
            }
        }
        fmt.Println()
    }
}
