package main

import (
    "fmt"
    "math/big"
)

// task 1: function returns list of derangements of n integers
func dList(n int) (r [][]int) {
    a := make([]int, n)
    for i := range a {
        a[i] = i
    }
    // recursive closure permutes a
    var recurse func(last int)
    recurse = func(last int) {
        if last == 0 {
            // bottom of recursion.  you get here once for each permutation.
            // test if permutation is deranged.
            for j, v := range a {
                if j == v {
                    return // no, ignore it
                }
            }
            // yes, save a copy
            r = append(r, append([]int{}, a...))
            return
        }
        for i := last; i >= 0; i-- {
            a[i], a[last] = a[last], a[i]
            recurse(last - 1)
            a[i], a[last] = a[last], a[i]
        }
    }
    recurse(n - 1)
    return
}

// task 3: function computes subfactorial of n
func subFact(n int) *big.Int {
    if n == 0 {
        return big.NewInt(1)
    } else if n == 1 {
        return big.NewInt(0)
    }
    d0 := big.NewInt(1)
    d1 := big.NewInt(0)
    f := new(big.Int)
    for i, n64 := int64(1), int64(n); i < n64; i++ {
        d0, d1 = d1, d0.Mul(f.SetInt64(i), d0.Add(d0, d1))
    }
    return d1
}

func main() {
    // task 2:
    fmt.Println("Derangements of 4 integers")
    for _, d := range dList(4) {
        fmt.Println(d)
    }

    // task 4:
    fmt.Println("\nNumber of derangements")
    fmt.Println("N  Counted  Calculated")
    for n := 0; n <= 9; n++ {
        fmt.Printf("%d %8d %11s\n", n, len(dList(n)), subFact(n).String())
    }

    // stretch (sic)
    fmt.Println("\n!20 =", subFact(20))
}
