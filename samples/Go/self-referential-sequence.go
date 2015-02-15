package main

import (
    "fmt"
    "strconv"
)

func main() {
    var maxLen int
    var seqMaxLen [][]string
    for n := 1; n < 1e6; n++ {
        switch s := seq(n); {
        case len(s) == maxLen:
            seqMaxLen = append(seqMaxLen, s)
        case len(s) > maxLen:
            maxLen = len(s)
            seqMaxLen = [][]string{s}
        }
    }
    fmt.Println("Max sequence length:", maxLen)
    fmt.Println("Sequences:", len(seqMaxLen))
    for _, seq := range seqMaxLen {
        fmt.Println("Sequence:")
        for _, t := range seq {
            fmt.Println(t)
        }
    }
}

func seq(n int) []string {
    s := strconv.Itoa(n)
    ss := []string{s}

    for {
        dSeq := sortD(s)
        d := dSeq[0]
        nd := 1
        s = ""
        for i := 1; ; i++ {
            if i == len(dSeq) {
                s = fmt.Sprintf("%s%d%c", s, nd, d)
                break
            }
            if dSeq[i] == d {
                nd++
            } else {
                s = fmt.Sprintf("%s%d%c", s, nd, d)
                d = dSeq[i]
                nd = 1
            }
        }
        for _, s0 := range ss {
            if s == s0 {
                return ss
            }
        }
        ss = append(ss, s)
    }
    panic("unreachable")
}

func sortD(s string) []rune {
    r := make([]rune, len(s))
    for i, d := range s {
        j := 0
        for ; j < i; j++ {
            if d > r[j] {
                copy(r[j+1:], r[j:i])
                break
            }
        }
        r[j] = d
    }
    return r
}
