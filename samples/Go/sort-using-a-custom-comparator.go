package main

import (
    "fmt"
    "sort"
    "strings"
)

type sortable []string

func (s sortable) Len() int      { return len(s) }
func (s sortable) Swap(i, j int) { s[i], s[j] = s[j], s[i] }
func (s sortable) Less(i, j int) bool {
    a, b := s[i], s[j]
    if len(a) != len(b) {
        return len(a) > len(b)
    }
    return strings.ToLower(a) < strings.ToLower(b)
}

func main() {
    var s sortable = strings.Fields("To tell your name the livelong day To an admiring bog")
    fmt.Println(s, "(original)")

    sort.Sort(s)
    fmt.Println(s, "(sorted)")
}
