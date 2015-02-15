package main

import (
    "fmt"
    "strconv"
)

func happy(n int) bool {
    m := make(map[int]int)
    for n > 1 {
        m[n] = 0
        s := strconv.Itoa(n)
        n = 0
        for _, d := range s {
            x := int(d) - '0'
            n += x * x
        }
        if _, ok := m[n]; ok {
            return false
        }
    }
    return true
}

func main() {
    for found, n := 0, 1; found < 8; n++ {
        if happy(n) {
            fmt.Print(n, " ")
            found++
        }
    }
    fmt.Println("")
}
