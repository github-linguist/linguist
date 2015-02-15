package main

import "fmt"

const ( // state:
    m   = iota // missing:  all elements missing so far
    c          // continuous:  all elements included so far are continuous
    cm         // one or more continuous followed by one or more missing
    cmc        // non-continuous subsequence
)

func ncs(s []int) [][]int {
    if len(s) < 3 {
        return nil
    }
    return append(n2(nil, s[1:], m), n2([]int{s[0]}, s[1:], c)...)
}

var skip = []int{m, cm, cm, cmc}
var incl = []int{c, c, cmc, cmc}

func n2(ss, tail []int, seq int) [][]int {
    if len(tail) == 0 {
        if seq != cmc {
            return nil
        }
        return [][]int{ss}
    }
    return append(n2(append([]int{}, ss...), tail[1:], skip[seq]),
        n2(append(ss, tail[0]), tail[1:], incl[seq])...)
}

func main() {
    ss := ncs([]int{1, 2, 3, 4})
    fmt.Println(len(ss), "non-continuous subsequences:")
    for _, s := range ss {
        fmt.Println("  ", s)
    }
}
