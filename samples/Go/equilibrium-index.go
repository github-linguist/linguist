package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    fmt.Println(ex([]int32{-7, 1, 5, 2, -4, 3, 0}))

    // sequence of 1,000,000 random numbers, with values
    // chosen so that it will be likely to have a couple
    // of equalibrium indexes.
    rand.Seed(time.Now().UnixNano())
    verylong := make([]int32, 1e6)
    for i := range verylong {
        verylong[i] = rand.Int31n(1001) - 500
    }
    fmt.Println(ex(verylong))
}

func ex(s []int32) (eq []int) {
    var r, l int64
    for _, el := range s {
        r += int64(el)
    }
    for i, el := range s {
        r -= int64(el)
        if l == r {
            eq = append(eq, i)
        }
        l += int64(el)
    }
    return
}
