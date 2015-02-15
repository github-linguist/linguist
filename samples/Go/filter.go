package main

import (
    "fmt"
    "math/rand"
)

func main() {
    a := rand.Perm(20)
    fmt.Println(a)       // show array to filter
    fmt.Println(even(a)) // show result of non-destructive filter
    fmt.Println(a)       // show that original array is unchanged
    reduceToEven(&a)     // destructive filter
    fmt.Println(a)       // show that a is now changed
    // a is not only changed, it is changed in place.  length and capacity
    // show that it still has its original allocated capacity but has now
    // been reduced in length.
    fmt.Println("a len:", len(a), "cap:", cap(a))
}

func even(a []int) (r []int) {
    for _, e := range a {
        if e%2 == 0 {
            r = append(r, e)
        }
    }
    return
}

func reduceToEven(pa *[]int) {
    a := *pa
    var last int
    for _, e := range a {
        if e%2 == 0 {
            a[last] = e
            last++
        }
    }
    *pa = a[:last]
}
