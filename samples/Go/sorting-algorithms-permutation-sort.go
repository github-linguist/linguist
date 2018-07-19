package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}

// in place permutation sort of slice a
func main() {
    fmt.Println("before:", a)
    if len(a) > 1 && !recurse(len(a) - 1) {
        // recurse should never return false from the top level.
        // if it does, it means some code somewhere is busted,
        // either the the permutation generation code or the
        // sortedness testing code.
        panic("sorted permutation not found!")
    }
    fmt.Println("after: ", a)
}

// recursive permutation generator
func recurse(last int) bool {
    if last <= 0 {
        // bottom of recursion.  test if sorted.
        for i := len(a) - 1; a[i] >= a[i-1]; i-- {
            if i == 1 {
                return true
            }
        }
        return false
    }
    for i := 0; i <= last; i++ {
        a[i], a[last] = a[last], a[i]
        if recurse(last - 1) {
            return true
        }
        a[i], a[last] = a[last], a[i]
    }
    return false
}
