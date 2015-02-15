package main

import (
    "fmt"
    "math/cmplx"
)

func main() {
    a := 1 + 1i
    b := 3.14159 + 1.25i
    fmt.Println("a:      ", a)
    fmt.Println("b:      ", b)
    fmt.Println("a + b:  ", a+b)
    fmt.Println("a * b:  ", a*b)
    fmt.Println("-a:     ", -a)
    fmt.Println("1 / a:  ", 1/a)
    fmt.Println("a̅:      ", cmplx.Conj(a))
}
