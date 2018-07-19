package main

import (
    "fmt"
)

func main() {
    comb(5, 3, func(c []int) {
        fmt.Println(c)
    })
}

func comb(n, m int, emit func([]int)) {
    s := make([]int, m)
    last := m - 1
    var rc func(int, int)
    rc = func(i, next int) {
        for j := next; j < n; j++ {
            s[i] = j
            if i == last {
                emit(s)
            } else {
                rc(i+1, j+1)
            }
        }
        return
    }
    rc(0, 0)
}
