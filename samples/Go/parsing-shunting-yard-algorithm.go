package main

import (
    "fmt"
    "strings"
)

var input = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

var opa = map[string]struct {
    prec   int
    rAssoc bool
}{
    "^": {4, true},
    "*": {3, false},
    "/": {3, false},
    "+": {2, false},
    "-": {2, false},
}

func main() {
    fmt.Println("infix:  ", input)
    fmt.Println("postfix:", parseInfix(input))
}

func parseInfix(e string) (rpn string) {
    var stack []string // holds operators and left parenthesis
    for _, tok := range strings.Fields(e) {
        switch tok {
        case "(":
            stack = append(stack, tok) // push "(" to stack
        case ")":
            var op string
            for {
                // pop item ("(" or operator) from stack
                op, stack = stack[len(stack)-1], stack[:len(stack)-1]
                if op == "(" {
                    break // discard "("
                }
                rpn += " " + op // add operator to result
            }
        default:
            if o1, isOp := opa[tok]; isOp {
                // token is an operator
                for len(stack) > 0 {
                    // consider top item on stack
                    op := stack[len(stack)-1]
                    if o2, isOp := opa[op]; !isOp || o1.prec > o2.prec ||
                        o1.prec == o2.prec && o1.rAssoc {
                        break
                    }
                    // top item is an operator that needs to come off
                    stack = stack[:len(stack)-1] // pop it
                    rpn += " " + op              // add it to result
                }
                // push operator (the new one) to stack
                stack = append(stack, tok)
            } else { // token is an operand
                if rpn > "" {
                    rpn += " "
                }
                rpn += tok // add operand to result
            }
        }
    }
    // drain stack to result
    for len(stack) > 0 {
        rpn += " " + stack[len(stack)-1]
        stack = stack[:len(stack)-1]
    }
    return
}
