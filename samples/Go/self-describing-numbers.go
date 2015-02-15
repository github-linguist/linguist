package main

import (
    "fmt"
    "strconv"
    "strings"
)

// task 1 requirement
func sdn(n int64) bool {
    if n >= 1e10 {
        return false
    }
    s := strconv.FormatInt(n, 10)
    for d, p := range s {
        if int(p)-'0' != strings.Count(s, strconv.Itoa(d)) {
            return false
        }
    }
    return true
}

// task 2 code (takes a while to run)
func main() {
    for n := int64(0); n < 1e10; n++ {
        if sdn(n) {
            fmt.Println(n)
        }
    }
}
