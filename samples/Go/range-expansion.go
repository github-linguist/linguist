package main

import (
    "fmt"
    "strconv"
    "strings"
)

const input = "-6,-3--1,3-5,7-11,14,15,17-20"

func main() {
    fmt.Println("range:", input)
    var r []int
    var last int
    for _, part := range strings.Split(input, ",") {
        if i := strings.Index(part[1:], "-"); i == -1 {
            n, err := strconv.Atoi(part)
            if err != nil {
                fmt.Println(err)
                return
            }
            if len(r) > 0 {
                if last == n {
                    fmt.Println("duplicate value:", n)
                    return
                } else if last > n {
                    fmt.Println("values not ordered:", last, ">", n)
                    return
                }
            }
            r = append(r, n)
            last = n
        } else {
            n1, err := strconv.Atoi(part[:i+1])
            if err != nil {
                fmt.Println(err)
                return
            }
            n2, err := strconv.Atoi(part[i+2:])
            if err != nil {
                fmt.Println(err)
                return
            }
            if n2 < n1+2 {
                fmt.Println("invalid range:", part)
                return
            }
            if len(r) > 0 {
                if last == n1 {
                    fmt.Println("duplicate value:", n1)
                    return
                } else if last > n1 {
                    fmt.Println("values not ordered:", last, ">", n1)
                    return
                }
            }
            for i = n1; i <= n2; i++ {
                r = append(r, i)
            }
            last = n2
        }
    }
    fmt.Println("expanded:", r)
}
