package main

import "fmt"

// following function satisfies the task, returning true for all
// perfect numbers representable in the argument type
func isPerfect(n int64) bool {
    switch n {
    case 6, 28, 496, 8128, 33550336, 8589869056,
        137438691328, 2305843008139952128:
        return true
    }
    return false
}

// validation
func main() {
    for n := int64(1); ; n++ {
        if isPerfect(n) != computePerfect(n) {
            panic("bug")
        }
        if n%1e3 == 0 {
            fmt.Println("tested", n)
        }
    }
}

func computePerfect(n int64) bool {
    var sum int64
    for i := int64(1); i < n; i++ {
        if n%i == 0 {
            sum += i
        }
    }
    return sum == n
}
