package main

import "fmt"

func main() {
    printFactors(-1)
    printFactors(0)
    printFactors(1)
    printFactors(2)
    printFactors(3)
    printFactors(53)
    printFactors(45)
    printFactors(64)
    printFactors(600851475143)
    printFactors(999999999999999989)
}

func printFactors(nr int64) {
    if nr < 1 {
        fmt.Println("\nFactors of", nr, "not computed")
        return
    }
    fmt.Printf("\nFactors of %d: ", nr)
    fs := make([]int64, 1)
    fs[0] = 1
    apf := func(p int64, e int) {
        n := len(fs)
        for i, pp := 0, p; i < e; i, pp = i+1, pp*p {
            for j := 0; j < n; j++ {
                fs = append(fs, fs[j]*pp)
            }
        }
    }
    e := 0
    for ; nr & 1 == 0; e++ {
        nr >>= 1
    }
    apf(2, e)
    for d := int64(3); nr > 1; d += 2 {
        if d*d > nr {
            d = nr
        }
        for e = 0; nr%d == 0; e++ {
            nr /= d
        }
        if e > 0 {
            apf(d, e)
        }
    }
    fmt.Println(fs)
    fmt.Println("Number of factors =", len(fs))
}
