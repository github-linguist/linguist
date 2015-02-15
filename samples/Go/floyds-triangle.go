package main

import "fmt"

func main() {
    floyd(5)
    floyd(14)
}

func floyd(n int) {
    fmt.Printf("Floyd %d:\n", n)
    lowerLeftCorner := n*(n-1)/2 + 1
    lastInColumn := lowerLeftCorner
    lastInRow := 1
    for i, row := 1, 1; row <= n; i++ {
        w := len(fmt.Sprint(lastInColumn))
        if i < lastInRow {
            fmt.Printf("%*d ", w, i)
            lastInColumn++
        } else {
            fmt.Printf("%*d\n", w, i)
            row++
            lastInRow += row
            lastInColumn = lowerLeftCorner
        }
    }
}
