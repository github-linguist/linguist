package main

import (
    "fmt"
    "math"
    "strconv"
    "strings"
)

var input = "3 4 2 * 1 5 - 2 3 ^ ^ / +"

func main() {
    fmt.Printf("For postfix %q\n", input)
    fmt.Println("\nToken            Action            Stack")
    var stack []float64
    for _, tok := range strings.Fields(input) {
        action := "Apply op to top of stack"
        switch tok {
        case "+":
            stack[len(stack)-2] += stack[len(stack)-1]
            stack = stack[:len(stack)-1]
        case "-":
            stack[len(stack)-2] -= stack[len(stack)-1]
            stack = stack[:len(stack)-1]
        case "*":
            stack[len(stack)-2] *= stack[len(stack)-1]
            stack = stack[:len(stack)-1]
        case "/":
            stack[len(stack)-2] /= stack[len(stack)-1]
            stack = stack[:len(stack)-1]
        case "^":
            stack[len(stack)-2] =
                math.Pow(stack[len(stack)-2], stack[len(stack)-1])
            stack = stack[:len(stack)-1]
        default:
            action = "Push num onto top of stack"
            f, _ := strconv.ParseFloat(tok, 64)
            stack = append(stack, f)
        }
        fmt.Printf("%3s    %-26s  %v\n", tok, action, stack)
    }
    fmt.Println("\nThe final value is", stack[0])
}
