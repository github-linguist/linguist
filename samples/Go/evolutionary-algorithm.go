package main

import (
    "fmt"
    "math/rand"
    "time"
)

var target = []byte("METHINKS IT IS LIKE A WEASEL")
var set = []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZ ")
var parent []byte

func init() {
    rand.Seed(time.Now().UnixNano())
    parent = make([]byte, len(target))
    for i := range parent {
        parent[i] = set[rand.Intn(len(set))]
    }
}

// fitness:  0 is perfect fit.  greater numbers indicate worse fit.
func fitness(a []byte) (h int) {
    // (hamming distance)
    for i, tc := range target {
        if a[i] != tc {
            h++
        }
    }
    return
}

// set m to mutation of p, with each character of p mutated with probability r
func mutate(p, m []byte, r float64) {
    for i, ch := range p {
        if rand.Float64() < r {
            m[i] = set[rand.Intn(len(set))]
        } else {
            m[i] = ch
        }
    }
}

func main() {
    const c = 20 // number of times to copy and mutate parent

    copies := make([][]byte, c)
    for i := range copies {
        copies[i] = make([]byte, len(parent))
    }

    fmt.Println(string(parent))
    for best := fitness(parent); best > 0; {
        for _, cp := range copies {
            mutate(parent, cp, .05)
        }
        for _, cp := range copies {
            fm := fitness(cp)
            if fm < best {
                best = fm
                copy(parent, cp)
                fmt.Println(string(parent))
            }
        }
    }
}
