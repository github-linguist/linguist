package main

import (
    "fmt"
)

func main() {
    a := []int{0, 1, 1} // ignore 0 element. work 1 based.
    x := 1  // last number in list
    n := 2  // index of last number in list = len(a)-1
    mallow := 0
    for p := 1; p < 20; p++ {
        max := 0.
        for nextPot := n*2; n < nextPot; {
            n = len(a) // advance n
            x = a[x]+a[n-x]
            a = append(a, x)
            f := float64(x)/float64(n)
            if f > max {
                max = f
            }
            if f >= .55 {
                mallow = n
            }
        }
        fmt.Printf("max between 2^%d and 2^%d was %f\n", p, p+1, max)
    }
    fmt.Println("winning number", mallow)
}
