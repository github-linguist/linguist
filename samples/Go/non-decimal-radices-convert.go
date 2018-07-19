package main

import (
    "fmt"
    "math/big"
    "strconv"
)

func main () {
    s := strconv.FormatInt(26, 16) // returns the string "1a"
    fmt.Println(s)

    i, err := strconv.ParseInt("1a", 16, 64) // returns the integer (int64) 26
    if err == nil {
        fmt.Println(i)
    }
    b, ok := new(big.Int).SetString("1a", 16) // returns the big integer 26
    if ok {
        fmt.Println(b)
    }
}
