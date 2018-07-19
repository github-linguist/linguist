package main

import (
    "fmt"
    "math/rand"
    "time"
)

var ts = []string{"abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"}

func main() {
    rand.Seed(time.Now().UnixNano())
    for _, s := range ts {
        // create shuffled byte array of original string
        t := make([]byte, len(s))
        for i, r := range rand.Perm(len(s)) {
            t[i] = s[r]
        }
        // algorithm of Icon solution
        for i := 0; i < len(s); i++ {
            for j := 0; j < len(s); j++ {
                if i != j && t[i] != s[j] && t[j] != s[i] {
                    t[i], t[j] = t[j], t[i]
                    break
                }
            }
        }
        // count unchanged and output
        var count int
        for i, ic := range t {
            if ic == s[i] {
                count++
            }
        }
        fmt.Printf("%s -> %s (%d)\n", s, string(t), count)
    }
}
