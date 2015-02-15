package main

import (
    "fmt"
    "strings"
)

func stripchars(str, chr string) string {
    return strings.Map(func(r rune) rune {
        if strings.IndexRune(chr, r) < 0 {
            return r
        }
        return -1
    }, str)
}

func main() {
    fmt.Println(stripchars("She was a soul stripper. She took my heart!",
        "aei"))
}
