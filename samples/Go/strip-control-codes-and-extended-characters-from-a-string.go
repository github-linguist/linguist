package main

import (
    "fmt"
    "strings"
)

// two byte-oriented functions identical except for operator comparing c to 127.
func stripCtlFromBytes(str string) string {
    b := make([]byte, len(str))
    var bl int
    for i := 0; i < len(str); i++ {
        c := str[i]
        if c >= 32 && c != 127 {
            b[bl] = c
            bl++
        }
    }
    return string(b[:bl])
}

func stripCtlAndExtFromBytes(str string) string {
    b := make([]byte, len(str))
    var bl int
    for i := 0; i < len(str); i++ {
        c := str[i]
        if c >= 32 && c < 127 {
            b[bl] = c
            bl++
        }
    }
    return string(b[:bl])
}

// two UTF-8 functions identical except for operator comparing c to 127
func stripCtlFromUTF8(str string) string {
    return strings.Map(func(r rune) rune {
        if r >= 32 && r != 127 {
            return r
        }
        return -1
    }, str)
}

func stripCtlAndExtFromUTF8(str string) string {
    return strings.Map(func(r rune) rune {
        if r >= 32 && r < 127 {
            return r
        }
        return -1
    }, str)
}

const src = "déjà vu" + // precomposed unicode
    "\n\000\037 \041\176\177\200\377\n" + // various boundary cases
    "as⃝df̅" // unicode combining characters

func main() {
    fmt.Println("source text:")
    fmt.Println(src, "\n")
    fmt.Println("as bytes, stripped of control codes:")
    fmt.Println(stripCtlFromBytes(src), "\n")
    fmt.Println("as bytes, stripped of control codes and extended characters:")
    fmt.Println(stripCtlAndExtFromBytes(src), "\n")
    fmt.Println("as UTF-8, stripped of control codes:")
    fmt.Println(stripCtlFromUTF8(src), "\n")
    fmt.Println("as UTF-8, stripped of control codes and extended characters:")
    fmt.Println(stripCtlAndExtFromUTF8(src))
}
