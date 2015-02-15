package main

import (
    "fmt"
    "math"
)

const s = "1223334444"

func main() {
    m := map[rune]float64{}
    for _, r := range s {
        m[r]++
    }
    hm := 0.
    for _, c := range m {
        hm += c * math.Log2(c)
    }
    const l = float64(len(s))
    fmt.Println(math.Log2(l) - hm/l)
}
