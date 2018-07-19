package main

import (
    "fmt"
    "strings"
)

const input = `49927398716
49927398717
1234567812345678
1234567812345670`

var t = [...]int{0, 2, 4, 6, 8, 1, 3, 5, 7, 9}

func luhn(s string) bool {
    odd := len(s) & 1
    var sum int
    for i, c := range s {
        if c < '0' || c > '9' {
            return false
        }
        if i&1 == odd {
            sum += t[c-'0']
        } else {
            sum += int(c - '0')
        }
    }
    return sum%10 == 0
}

func main() {
    for _, s := range strings.Split(input, "\n") {
        fmt.Println(s, luhn(s))
    }
}
