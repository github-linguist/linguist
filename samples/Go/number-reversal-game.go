package main

import (
    "fmt"
    "math/rand"
    "sort"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())
    var k []int
    for {
        k = rand.Perm(9)
        for i, r := range k {
            if r == 0 {
                k[i] = 9
            }
        }
        if !sort.IntsAreSorted(k) {
            break
        }
    }
    fmt.Println("Sort digits by reversing a number of digits on the left.")
    var n, score int
    for {
        fmt.Print("Digits: ", k, ". How many to reverse? ")
        i, _ := fmt.Scanln(&n)
        score++
        if i == 0 || n < 2 || n > 9 {
            fmt.Println("\n(Enter a number from 2 to 9)")
            continue
        }
        for l, r := 0, n-1; l < r; l, r = l+1, r-1 {
            k[l], k[r] = k[r], k[l]
        }
        if sort.IntsAreSorted(k) {
            fmt.Print("Digits: ", k, ".\n")
            fmt.Print("Your score: ", score, ".  Good job.\n")
            return
        }
    }
}
