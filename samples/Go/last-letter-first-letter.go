package main

import (
    "fmt"
    "strings"
)

var pokemon = `audino bagon baltoy...67 names omitted...`

func main() {
    // split text into slice representing directed graph
    var d []string
    for _, l := range strings.Split(pokemon, "\n") {
        d = append(d, strings.Fields(l)...)
    }
    fmt.Println("searching", len(d), "names...")
    // try each name as possible start
    for i := range d {
        d[0], d[i] = d[i], d[0]
        search(d, 1, len(d[0]))
        d[0], d[i] = d[i], d[0]
    }
    fmt.Println("maximum path length:", len(ex))
    fmt.Println("paths of that length:", nMax)
    fmt.Print("example path of that length:")
    for i, n := range ex {
        if i%6 == 0 {
            fmt.Print("\n   ")
        }
        fmt.Print(n, " ")
    }
    fmt.Println()
}

var ex []string
var nMax int

func search(d []string, i, ncPath int) {
    // tally statistics
    if i == len(ex) {
        nMax++
    } else if i > len(ex) {
        nMax = 1
        ex = append(ex[:0], d[:i]...)
    }
    // recursive search
    lastName := d[i-1]
    lastChar := lastName[len(lastName)-1]
    for j := i; j < len(d); j++ {
        if d[j][0] == lastChar {
            d[i], d[j] = d[j], d[i]
            search(d, i+1, ncPath+1+len(d[i]))
            d[i], d[j] = d[j], d[i]
        }
    }
}
