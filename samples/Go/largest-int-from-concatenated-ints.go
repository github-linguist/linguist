// Variation of method 3.  Repeat digits to at least the size of the longest,
// then sort as strings.
package main

import (
    "fmt"
    "math/big"
    "sort"
    "strconv"
    "strings"
)

type c struct {
    i     int
    s, rs string
}

type cc []*c

func (c cc) Len() int           { return len(c) }
func (c cc) Less(i, j int) bool { return c[j].rs < c[i].rs }
func (c cc) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }

// Function required by task.  Takes a list of integers, returns big int.
func li(is ...int) *big.Int {
    ps := make(cc, len(is))
    ss := make([]c, len(is))
    ml := 0
    for j, i := range is {
        p := &ss[j]
        ps[j] = p
        p.i = i
        p.s = strconv.Itoa(i)
        if len(p.s) > ml {
            ml = len(p.s)
        }
    }
    for _, p := range ps {
        p.rs = strings.Repeat(p.s, (ml+len(p.s)-1)/len(p.s))
    }
    sort.Sort(ps)
    s := make([]string, len(ps))
    for i, p := range ps {
        s[i] = p.s
    }
    b, _ := new(big.Int).SetString(strings.Join(s, ""), 10)
    return b
}

func main() {
    fmt.Println(li(1, 34, 3, 98, 9, 76, 45, 4))
    fmt.Println(li(54, 546, 548, 60))
}
