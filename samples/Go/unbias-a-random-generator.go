package main

import (
    "fmt"
    "math/rand"
)

const samples = 1e6

func main() {
    fmt.Println("Generator  1 count  0 count  % 1 count")
    for n := 3; n <= 6; n++ {
        // function randN, per task description
        randN := func() int {
            if rand.Intn(n) == 0 {
                return 1
            }
            return 0
        }
        var b [2]int
        for x := 0; x < samples; x++ {
            b[randN()]++
        }
        fmt.Printf("randN(%d)   %7d  %7d    %5.2f%%\n",
            n, b[1], b[0], float64(b[1])*100/samples)

        // function unbiased, per task description
        unbiased := func() (b int) {
            for b = randN(); b == randN(); b = randN() {
            }
            return
        }
        var u [2]int
        for x := 0; x < samples; x++ {
            u[unbiased()]++
        }
        fmt.Printf("unbiased   %7d  %7d    %5.2f%%\n",
            u[1], u[0], float64(u[1])*100/samples)
    }
}
