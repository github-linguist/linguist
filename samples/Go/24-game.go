package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().Unix())
    n := make([]rune, 4)
    for i := range n {
        n[i] = rune(rand.Intn(9) + '1')
    }
    fmt.Printf("Your numbers: %c\n", n)
    fmt.Print("Enter RPN: ")
    var expr string
    fmt.Scan(&expr)
    if len(expr) != 7 {
        fmt.Println("invalid. expression length must be 7." +
            " (4 numbers, 3 operators, no spaces)")
        return
    }
    stack := make([]float64, 0, 4)
    for _, r := range expr {
        if r >= '0' && r <= '9' {
            if len(n) == 0 {
                fmt.Println("too many numbers.")
                return
            }
            i := 0
            for n[i] != r {
                i++
                if i == len(n) {
                    fmt.Println("wrong numbers.")
                    return
                }
            }
            n = append(n[:i], n[i+1:]...)
            stack = append(stack, float64(r-'0'))
            continue
        }
        if len(stack) < 2 {
            fmt.Println("invalid expression syntax.")
            return
        }
        switch r {
        case '+':
            stack[len(stack)-2] += stack[len(stack)-1]
        case '-':
            stack[len(stack)-2] -= stack[len(stack)-1]
        case '*':
            stack[len(stack)-2] *= stack[len(stack)-1]
        case '/':
            stack[len(stack)-2] /= stack[len(stack)-1]
        default:
            fmt.Printf("%c invalid.\n", r)
            return
        }
        stack = stack[:len(stack)-1]
    }
    if math.Abs(stack[0]-24) > 1e-6 {
        fmt.Println("incorrect.", stack[0], "!= 24")
    } else {
        fmt.Println("correct.")
    }
}
