package main

import (
    "fmt"
    "runtime/debug"
)

func main() {
    // to print it to standard error
    debug.PrintStack()
    // alternately to get it in a variable:
    stackTrace := debug.Stack()
    fmt.Printf("(%d bytes)\n", len(stackTrace))
}
