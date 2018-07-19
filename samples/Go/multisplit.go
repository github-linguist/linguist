package main

import (
    "fmt"
    "strings"
)

func ms(txt string, sep []string) (ans []string) {
    for txt > "" {
        sepMatch := ""
        posMatch := len(txt)
        for _, s := range sep {
            if p := strings.Index(txt, s); p >= 0 && p < posMatch {
                sepMatch = s
                posMatch = p
            }
        }
        ans = append(ans, txt[:posMatch])
        txt = txt[posMatch+len(sepMatch):]
    }
    return
}

func main() {
    fmt.Printf("%q\n", ms("a!===b=!=c", []string{"==", "!=", "="}))
}
