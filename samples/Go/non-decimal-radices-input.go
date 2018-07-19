package main

import (
    "bytes"
    "fmt"
    "math/big"
    "strconv"
)

func main() {
    // package strconv:  the most common string to int conversion,
    // base 10 only.
    x, _ := strconv.Atoi("13")
    fmt.Println(x)

    // ParseInt handles arbitrary bases from 2 to 36, and returns
    // a result of the requested size (64 bits shown here.)
    x64, _ := strconv.ParseInt("3c2", 19, 64)
    fmt.Println(x64)

    // package bytes+fmt:  allows direct conversion from strings
    // to integer types for bases 2, 8, 10, and 16.
    // (Fscanf and scanf are more common for reading from
    // files or stdin than for reading from strings.)
    fmt.Fscanf(bytes.NewBufferString("1101"), "%b", &x)
    fmt.Println(x)

    fmt.Fscanf(bytes.NewBufferString("15"), "%o", &x)
    fmt.Println(x)

    fmt.Fscanf(bytes.NewBufferString("13"), "%d", &x)
    fmt.Println(x)

    fmt.Fscanf(bytes.NewBufferString("d"), "%x", &x)
    fmt.Println(x)

    // package big:  allows conversion from string to big integer.
    // any base from 2 to 16 can be specified as second parameter.
    var z big.Int
    z.SetString("111", 3)
    fmt.Println(&z)

    // if second parameter is 0, base is determined by prefix, if any
    z.SetString("0b1101", 0) // 0b -> base 2
    fmt.Println(&z)

    z.SetString("015", 0) // 0 -> base 8
    fmt.Println(&z)

    z.SetString("13", 0) // no prefix -> base 10
    fmt.Println(&z)

    z.SetString("0xd", 0) // 0x -> base 16
    fmt.Println(&z)
}
