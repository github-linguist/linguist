package main

import "fmt"

func divCheck(x, y int) (q int, ok bool) {
    defer func() {
        recover()
    }()
    q = x / y
    return q, true
}

func main() {
    fmt.Println(divCheck(3, 2))
    fmt.Println(divCheck(3, 0))
}
