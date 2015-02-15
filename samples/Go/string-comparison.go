package main

import (
    "fmt"
    "strings"
)

func main() {
    // Go language string comparison operators:
    c := "cat"
    d := "dog"
    if c == d {
        fmt.Println(c, "is bytewise identical to", d)
    }
    if c != d {
        fmt.Println(c, "is bytewise different from", d)
    }
    if c > d {
        fmt.Println(c, "is lexically bytewise greater than", d)
    }
    if c < d {
        fmt.Println(c, "is lexically bytewise less than", d)
    }
    if c >= d {
        fmt.Println(c, "is lexically bytewise greater than or equal to", d)
    }
    if c <= d {
        fmt.Println(c, "is lexically bytewise less than or equal to", d)
    }
    // Go is strongly typed and will not directly compare a value of string
    // type to a value of numeric type.

    // A case insensitive compare can be done with a function in the strings
    // package in the Go standard library:
    eqf := `when interpreted as UTF-8 and compared under Unicode
simple case folding rules.`
    if strings.EqualFold(c, d) {
        fmt.Println(c, "equal to", d, eqf)
    } else {
        fmt.Println(c, "not equal to", d, eqf)
    }

    // Seeing that the built in operators work bytewise and the library
    // case folding functions interpret UTF-8, you might then ask about
    // other equality and inequality tests that interpret UTF-8.
    // Functions for this are not in the Go standard library but are in
    // the Go "sub repository" at code.google.com/p/go.  There is support
    // for Unicode normalization, collation tables, and locale sensitive
    // comparisons.
}
