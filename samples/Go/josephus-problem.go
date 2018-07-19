package main

import "fmt"

// basic task function
func finalSurvivor(n, k int) int {
    // argument validation omitted
    circle := make([]int, n)
    for i := range circle {
        circle[i] = i
    }
    k--
    exPos := 0
    for len(circle) > 1 {
        exPos = (exPos + k) % len(circle)
        circle = append(circle[:exPos], circle[exPos+1:]...)
    }
    return circle[0]
}

// extra
func position(n, k, pos int) int {
    // argument validation omitted
    circle := make([]int, n)
    for i := range circle {
        circle[i] = i
    }
    k--
    exPos := 0
    for len(circle) > 1 {
        exPos = (exPos + k) % len(circle)
        if pos == 0 {
            return circle[exPos]
        }
        pos--
        circle = append(circle[:exPos], circle[exPos+1:]...)
    }
    return circle[0]
}

func main() {
    // show basic task function on given test case
    fmt.Println(finalSurvivor(41, 3))
    // show extra function on all positions of given test case
    fmt.Println("Position  Prisoner")
    for i := 0; i < 41; i++ {
        fmt.Printf("%5d%10d\n", i, position(41, 3, i))
    }
}
