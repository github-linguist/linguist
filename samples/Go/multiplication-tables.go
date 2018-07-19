package main

import (
    "fmt"
)

func main() {
    fmt.Print(" x |")
    for i := 1; i <= 12; i++ {
        fmt.Printf("%4d", i)
    }
    fmt.Print("\n---+")
    for i := 1; i <= 12; i++ {
        fmt.Print("----")
    }
    for j := 1; j <= 12; j++ {
        fmt.Printf("\n%2d |", j)
        for i := 1; i <= 12; i++ {
            if i >= j {
                fmt.Printf("%4d", i*j)
            } else {
                fmt.Print("    ")
            }
        }
    }
    fmt.Println("")
}
