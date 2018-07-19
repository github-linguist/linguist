package main

import (
    "fmt"
    "strings"
    "unicode/utf8"
)

var order = 3
var grain = "#"

func main() {
    carpet := []string{grain}
    for ; order > 0; order-- {
        // repeat expression allows for multiple character
        // grain and for multi-byte UTF-8 characters.
        hole := strings.Repeat(" ", utf8.RuneCountInString(carpet[0]))
        middle := make([]string, len(carpet))
        for i, s := range carpet {
            middle[i] = s + hole + s
            carpet[i] = strings.Repeat(s, 3)
        }
        carpet = append(append(carpet, middle...), carpet...)
    }
    for _, r := range carpet {
        fmt.Println(r)
    }
}
