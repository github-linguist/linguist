package main

import (
    "errors"
    "fmt"
)

func dot(x, y []int) (r int, err error) {
    if len(x) != len(y) {
        return 0, errors.New("incompatible lengths")
    }
    for i := range x {
        r += x[i] * y[i]
    }
    return
}

func main() {
    d, err := dot([]int{1, 3, -5}, []int{4, -2, -1})
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println(d)
}
