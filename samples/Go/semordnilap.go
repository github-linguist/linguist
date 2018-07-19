package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "strings"
)

func main() {
    // read file into memory as one big block
    data, err := ioutil.ReadFile("unixdict.txt")
    if err != nil {
        log.Fatal(err)
    }
    // copy the block, split it up into words
    words := strings.Split(string(data), "\n")
    // optional, free the first block for garbage collection
    data = nil
    // put words in a map, also determine length of longest word
    m := make(map[string]bool)
    longest := 0
    for _, w := range words {
        m[string(w)] = true
        if len(w) > longest {
            longest = len(w)
        }
    }
    // allocate a buffer for reversing words
    r := make([]byte, longest)
    // iterate over word list
    sem := 0
    var five []string
    for _, w := range words {
        // first, delete from map.  this prevents a palindrome from matching
        // itself, and also prevents it's reversal from matching later.
        delete(m, w)
        // use buffer to reverse word
        last := len(w) - 1
        for i := 0; i < len(w); i++ {
            r[i] = w[last-i]
        }
        rs := string(r[:len(w)])
        // see if reversed word is in map, accumulate results
        if m[rs] {
            sem++
            if len(five) < 5 {
                five = append(five, w+"/"+rs)
            }
        }
    }
    // print results
    fmt.Println(sem, "pairs")
    fmt.Println("examples:")
    for _, e := range five {
        fmt.Println("  ", e)
    }
}
