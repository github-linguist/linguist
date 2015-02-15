package main

import "fmt"

func foo() {
    fmt.Println("let's foo...")
    defer func() {
        if err := recover(); err != nil {
            fmt.Printf("Error: %v\n", err)
        }
    }()
    panic("FAIL!")
    fmt.Println("there's no point in going on.")
}

func main() {
    foo()
    fmt.Println("glad that's over.")
}
