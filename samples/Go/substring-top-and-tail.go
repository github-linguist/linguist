package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    // ASCII contents:  Interpreting "characters" as bytes.
    s := "ASCII"
    fmt.Println("String:                ", s)
    fmt.Println("First byte removed:    ", s[1:])
    fmt.Println("Last byte removed:     ", s[:len(s)-1])
    fmt.Println("First and last removed:", s[1:len(s)-1])
    // UTF-8 contents:  "Characters" as runes (unicode code points)
    u := "Δημοτική"
    fmt.Println("String:                ", u)
    _, sizeFirst := utf8.DecodeRuneInString(u)
    fmt.Println("First rune removed:    ", u[sizeFirst:])
    _, sizeLast := utf8.DecodeLastRuneInString(u)
    fmt.Println("Last rune removed:     ", u[:len(u)-sizeLast])
    fmt.Println("First and last removed:", u[sizeFirst:len(u)-sizeLast])
}
