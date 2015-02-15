package main

import (
    "fmt"
    "io/ioutil"
    "sort"
    "unicode"
)

const file = "unixdict.txt"

func main() {
    bs, err := ioutil.ReadFile(file)
    if err != nil {
        fmt.Println(err)
        return
    }
    m := make(map[rune]int)
    for _, r := range string(bs) {
        m[r]++
    }
    // answer is now in m.  sort and format output:
    lfs := make(lfList, 0, len(m))
    for l, f := range m {
        lfs = append(lfs, &letterFreq{l, f})
    }
    sort.Sort(lfs)
    fmt.Println("file:", file)
    fmt.Println("letter  frequency")
    for _, lf := range lfs {
        if unicode.IsGraphic(lf.rune) {
            fmt.Printf("   %c    %7d\n", lf.rune, lf.freq)
        } else {
            fmt.Printf("%U  %7d\n", lf.rune, lf.freq)
        }
    }
}

type letterFreq struct {
    rune
    freq int
}
type lfList []*letterFreq

func (lfs lfList) Len() int { return len(lfs) }
func (lfs lfList) Less(i, j int) bool {
    switch fd := lfs[i].freq - lfs[j].freq; {
    case fd < 0:
        return false
    case fd > 0:
        return true
    }
    return lfs[i].rune < lfs[j].rune
}
func (lfs lfList) Swap(i, j int) {
    lfs[i], lfs[j] = lfs[j], lfs[i]
}
