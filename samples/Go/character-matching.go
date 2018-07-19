package main

import (
    "fmt"
    "strings"
)

func match(first, second string) {
    fmt.Printf("1. %s starts with %s: %t\n",
        first, second, strings.HasPrefix(first, second))
    i := strings.Index(first, second)
    fmt.Printf("2. %s contains %s: %t,\n", first, second, i >= 0)
    if i >= 0 {
        fmt.Printf("2.1. at location %d,\n", i)
        for start := i+1;; {
            if i = strings.Index(first[start:], second); i < 0 {
                break
            }
            fmt.Printf("2.2. at location %d,\n", start+i)
            start += i+1
        }
        fmt.Println("2.2. and that's all")
    }
    fmt.Printf("3. %s ends with %s: %t\n",
        first, second, strings.HasSuffix(first, second))
}

func main() {
    match("abracadabra", "abr")
}
