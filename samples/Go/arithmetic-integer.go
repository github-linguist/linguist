package main

import "fmt"

func main() {
    var a, b int
    fmt.Print("enter two integers: ")
    fmt.Scanln(&a, &b)
    fmt.Printf("%d + %d = %d\n", a, b, a+b)
    fmt.Printf("%d - %d = %d\n", a, b, a-b)
    fmt.Printf("%d * %d = %d\n", a, b, a*b)
    fmt.Printf("%d / %d = %d\n", a, b, a/b)  // truncates towards 0
    fmt.Printf("%d %% %d = %d\n", a, b, a%b) // same sign as first operand
    // no exponentiation operator
}
