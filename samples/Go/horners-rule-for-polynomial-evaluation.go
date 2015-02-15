package main

import "fmt"

func horner(x int64, c []int64) (acc int64) {
    for i := len(c) - 1; i >= 0; i-- {
        acc = acc*x + c[i]
    }
    return
}

func main() {
    fmt.Println(horner(3, []int64{-19, 7, -4, 6}))
}
