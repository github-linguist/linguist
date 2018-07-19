package main

import (
    "fmt"
    "strings"
)

const commentChars = "#;"

func stripComment(source string) string {
    if cut := strings.IndexAny(source, commentChars); cut >= 0 {
        return source[:cut]
    }
    return source
}

func main() {
    for _, s := range []string{
        "apples, pears # and bananas",
        "apples, pears ; and bananas",
        "no bananas",
    } {
        fmt.Printf("source:   %q\n", s)
        fmt.Printf("stripped: %q\n", stripComment(s))
    }
}
