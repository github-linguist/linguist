package main

import (
    "fmt"
    "strings"
)

var tests = []string{
    "3 4 2 * 1 5 - 2 3 ^ ^ / +",
    "1 2 + 3 4 + ^ 5 6 + ^",
}

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

const nPrec = 9

func main() {
    for _, t := range tests {
        parseRPN(t)
    }
}

func parseRPN(e string) {
    fmt.Println("\npostfix:", e)
    type sf struct {
        prec int
        expr string
    }
    var stack []sf
    for _, tok := range strings.Fields(e) {
        fmt.Println("token:", tok)
        if op, isOp := opa[tok]; isOp {
            rhs := &stack[len(stack)-1]
            stack = stack[:len(stack)-1]
            lhs := &stack[len(stack)-1]
            if lhs.prec < op.prec || (lhs.prec == op.prec && op.rAssoc) {
                lhs.expr = "(" + lhs.expr + ")"
            }
            lhs.expr += " " + tok + " "
            if rhs.prec < op.prec || (rhs.prec == op.prec && !op.rAssoc) {
                lhs.expr += "(" + rhs.expr + ")"
            } else {
                lhs.expr += rhs.expr
            }
            lhs.prec = op.prec
        } else {
            stack = append(stack, sf{nPrec, tok})
        }
        for _, f := range stack {
            fmt.Printf("    %d %q\n", f.prec, f.expr)
        }
    }
    fmt.Println("infix:", stack[0].expr)
}
