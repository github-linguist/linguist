package main

import (
    "fmt"
    "math/rand"
    "time"
)

// constants define order of colors in Dutch national flag
const (
    red = iota
    white
    blue
    nColors
)

// zero object of type is valid red ball.
type ball struct {
    color int
}

// order of balls based on DNF
func (b1 ball) lt(b2 ball) bool {
    return b1.color < b2.color
}

// type for arbitrary ordering of balls
type ordering []ball

// predicate tells if balls are ordered by DNF
func (o ordering) ordered() bool {
    var b0 ball
    for _, b := range o {
        if b.lt(b0) {
            return false
        }
        b0 = b
    }
    return true
}

func init() {
    rand.Seed(time.Now().Unix())
}

// constructor returns new ordering of balls which is randomized but
// guaranteed to be not in DNF order.  function panics for n < 2.
func outOfOrder(n int) ordering {
    if n < 2 {
        panic(fmt.Sprintf("%d invalid", n))
    }
    r := make(ordering, n)
    for {
        for i, _ := range r {
            r[i].color = rand.Intn(nColors)
        }
        if !r.ordered() {
            break
        }
    }
    return r
}

// O(n) algorithm
// http://www.csse.monash.edu.au/~lloyd/tildeAlgDS/Sort/Flag/
func (a ordering) sort3() {
    lo, mid, hi := 0, 0, len(a)-1
    for mid <= hi {
        switch a[mid].color {
        case red:
            a[lo], a[mid] = a[mid], a[lo]
            lo++
            mid++
        case white:
            mid++
        default:
            a[mid], a[hi] = a[hi], a[mid]
            hi--
        }
    }
}

func main() {
    f := outOfOrder(12)
    fmt.Println(f)
    f.sort3()
    fmt.Println(f)
}
