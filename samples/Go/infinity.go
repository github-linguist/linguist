package main

import (
    "fmt"
    "math"
)

// function called for by task
func posInf() float64 {
    return math.Inf(1) // argument specifies positive infinity
}

func main() {
    x := 1.5 // type of x determined by literal
    // that this compiles demonstrates that PosInf returns same type as x,
    // the type specified by the task.
    x = posInf()                     // test function
    fmt.Println(x, math.IsInf(x, 1)) // demonstrate result
}
