package main

import (
    "fmt"
    "sort"
)

type pair struct {
    name, value string
}
type csArray []pair

// three methods satisfy sort.Interface
func (a csArray) Less(i, j int) bool { return a[i].name < a[j].name }
func (a csArray) Len() int           { return len(a) }
func (a csArray) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }

var x = csArray{
    pair{"joe", "120"},
    pair{"foo", "31"},
    pair{"bar", "251"},
}

func main() {
    sort.Sort(x)
    for _, p := range x {
        fmt.Printf("%5s: %s\n", p.name, p.value)
    }
}
