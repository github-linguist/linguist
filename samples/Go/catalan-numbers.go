package main

import (
    "fmt"
    "math/big"
)

func main() {
    var b, c big.Int
    for n := int64(0); n < 15; n++ {
        fmt.Println(c.Div(b.Binomial(n*2, n), c.SetInt64(n+1)))
    }
}
