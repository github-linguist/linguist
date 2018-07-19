package main

import "fmt"

func a(v bool) bool {
    fmt.Print("a")
    return v
}

func b(v bool) bool {
    fmt.Print("b")
    return v
}

func test(i, j bool) {
    fmt.Printf("Testing a(%t) && b(%t)\n", i, j)
    fmt.Print("Trace:  ")
    fmt.Println("\nResult:", a(i) && b(j))

    fmt.Printf("Testing a(%t) || b(%t)\n", i, j)
    fmt.Print("Trace:  ")
    fmt.Println("\nResult:", a(i) || b(j))

    fmt.Println("")
}

func main() {
    test(false, false)
    test(false, true)
    test(true, false)
    test(true, true)
}
