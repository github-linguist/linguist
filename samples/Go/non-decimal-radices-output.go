package main

import (
    "fmt"
    "math/big"
    "strconv"
)

func main() {
    // package strconv:
    // Itoa is the most common int to string conversion. it is base 10 only.
    x := strconv.Itoa(13)
    fmt.Printf("%q\n", x)

    // FormatInt handles arbitrary bases from 2 to 36.
    x = strconv.FormatInt(1313, 19)
    fmt.Printf("%q\n", x)

    // package fmt:  allows direct conversion from integer
    // to string types for bases 2, 8, 10, and 16.
    fmt.Printf("%b\n", 13)
    fmt.Printf("%o\n", 13)
    fmt.Printf("%d\n", 13)
    fmt.Printf("%x\n", 13)

    // package big:  string conversion is base 10 only.
    fmt.Println(big.NewInt(13))
}
