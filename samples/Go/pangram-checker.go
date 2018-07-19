package main

import "fmt"

func main() {
    for _, s := range []string{
        "The quick brown fox jumps over the lazy dog.",
        `Watch "Jeopardy!", Alex Trebek's fun TV quiz game.`,
        "Not a pangram.",
    } {
        if pangram(s) {
            fmt.Println("Yes:", s)
        } else {
            fmt.Println("No: ", s)
        }
    }
}

func pangram(s string) bool {
    var rep [26]bool
    var count int
    for _, c := range s {
        if c >= 'a' {
            if c > 'z' {
                continue
            }
            c -= 'a'
        } else {
            if c < 'A' || c > 'Z' {
                continue
            }
            c -= 'A'
        }
        if !rep[c] {
            if count == 25 {
                return true
            }
            rep[c] = true
            count++
        }
    }
    return false
}
