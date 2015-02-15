package main

import (
    "fmt"
    "strings"
    "unicode"
    "unicode/utf8"
)

func main() {
    show("alphaBETA")
    show("alpha BETA")
    // Three digraphs that should render similar to DZ, Lj, and nj.
    show("Ǆǈǌ")
    // Unicode apostrophe in third word.
    show("o'hare O'HARE o’hare don't")
}

func show(s string) {
    fmt.Println("\nstring:         ",
        s, " len:", utf8.RuneCountInString(s), "runes") // DZLjnj
    fmt.Println("All upper case: ", strings.ToUpper(s)) // DZLJNJ
    fmt.Println("All lower case: ", strings.ToLower(s)) // dzljnj
    fmt.Println("All title case: ", strings.ToTitle(s)) // DzLjNj
    fmt.Println("Title words:    ", strings.Title(s))   // Dzljnj
    fmt.Println("Swapping case:  ",                     // DzLjNJ
        strings.Map(unicode.SimpleFold, s))
}
