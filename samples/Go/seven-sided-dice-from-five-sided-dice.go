package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// "given"
func dice5() int {
    return rand.Intn(5) + 1
}

// function specified by task "Seven-sided dice from five-sided dice"
func dice7() (i int) {
    for {
        i = 5*dice5() + dice5()
        if i < 27 {
            break
        }
    }
    return (i / 3) - 1
}

// function specified by task "Verify distribution uniformity/Naive"
//
// Parameter "f" is expected to return a random integer in the range 1..n.
// (Values out of range will cause an unceremonious crash.)
// "Max" is returned as an "indication of distribution achieved."
// It is the maximum delta observed from the count representing a perfectly
// uniform distribution.
// Also returned is a boolean, true if "max" is less than threshold
// parameter "delta."
func distCheck(f func() int, n int,
    repeats int, delta float64) (max float64, flatEnough bool) {
    count := make([]int, n)
    for i := 0; i < repeats; i++ {
        count[f()-1]++
    }
    expected := float64(repeats) / float64(n)
    for _, c := range count {
        max = math.Max(max, math.Abs(float64(c)-expected))
    }
    return max, max < delta
}

// Driver, produces output satisfying both tasks.
func main() {
    rand.Seed(time.Now().UnixNano())
    const calls = 1000000
    max, flatEnough := distCheck(dice7, 7, calls, 500)
    fmt.Println("Max delta:", max, "Flat enough:", flatEnough)
    max, flatEnough = distCheck(dice7, 7, calls, 500)
    fmt.Println("Max delta:", max, "Flat enough:", flatEnough)
}
