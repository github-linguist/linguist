package main

import (
    "fmt"
    "sort"
    "strconv"
    "strings"
)

var data = `12 127 28 42` //...omitted...127 31 116 146`

func main() {
    // load data into map
    m := make(map[int][]string)
    for _, s := range strings.Fields(data) {
        if len(s) == 1 {
            m[0] = append(m[0], s)
        } else if i, err := strconv.Atoi(s[:len(s)-1]); err == nil {
            m[i] = append(m[i], s[len(s)-1:])
        } else {
            panic("non numeric data")
        }
    }
    // sort stem
    s := make([]int, len(m))
    var i int
    for k := range m {
        s[i] = k
        i++
    }
    sort.Ints(s)
    // print
    for k := s[0]; ; k++ {
        v := m[k]
        sort.Strings(v)
        fmt.Printf("%2d | %s\n", k, strings.Join(v, " "))
        if k == s[len(s)-1] {
            break
        }
    }
}
