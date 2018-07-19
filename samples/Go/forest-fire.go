package main

import (
    "fmt"
    "math/rand"
    "strings"
)

const (
    rows = 20
    cols = 30
    p    = .01
    f    = .001
)

const rx = rows + 2
const cx = cols + 2

func main() {
    odd := make([]byte, rx*cx)
    even := make([]byte, rx*cx)
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            if rand.Intn(2) == 1 {
                odd[r*cx+c] = 'T'
            }
        }
    }
    for {
        print(odd)
        step(even, odd)
        fmt.Scanln()

        print(even)
        step(odd, even)
        fmt.Scanln()
    }
}

func print(model []byte) {
    fmt.Println(strings.Repeat("__", cols))
    fmt.Println()
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            if model[r*cx+c] == 0 {
                fmt.Print("  ")
            } else {
                fmt.Printf(" %c", model[r*cx+c])
            }
        }
        fmt.Println()
    }
}

func step(dst, src []byte) {
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            x := r*cx + c
            dst[x] = src[x]
            switch dst[x] {
            case '#':
                // rule 1. A burning cell turns into an empty cell
                dst[x] = 0
            case 'T':
                // rule 2. A tree will burn if at least one neighbor is burning
                if src[x-cx-1]=='#'  || src[x-cx]=='#' || src[x-cx+1]=='#' ||
                    src[x-1] == '#'  ||                   src[x+1] == '#'  ||
                    src[x+cx-1]=='#' || src[x+cx]=='#' || src[x+cx+1] == '#' {
                    dst[x] = '#'

                    // rule 3. A tree ignites with probability f
                    // even if no neighbor is burning
                } else if rand.Float64() < f {
                    dst[x] = '#'
                }
            default:
                // rule 4. An empty space fills with a tree with probability p
                if rand.Float64() < p {
                    dst[x] = 'T'
                }
            }
        }
    }
}
