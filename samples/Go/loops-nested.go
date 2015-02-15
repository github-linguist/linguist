package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())

    values := make([][]int, 10)
    for i := range values {
        values[i] = make([]int, 10)
        for j := range values[i] {
            values[i][j] = rand.Intn(20) + 1
        }
    }

outerLoop:
    for i, row := range values {
        fmt.Printf("%3d)", i)
        for _, value := range row {
            fmt.Printf(" %3d", value)
            if value == 20 {
                break outerLoop
            }
        }
        fmt.Printf("\n")
    }
    fmt.Printf("\n")
}
