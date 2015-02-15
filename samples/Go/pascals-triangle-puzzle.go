package main

import "fmt"

// representation of an expression in x, y, and z
type expr struct {
    x, y, z float64 // coefficients
    c       float64 // constant term
}

// add two expressions
func addExpr(a, b expr) expr {
    return expr{a.x + b.x, a.y + b.y, a.z + b.z, a.c + b.c}
}

// subtract two expressions
func subExpr(a, b expr) expr {
    return expr{a.x - b.x, a.y - b.y, a.z - b.z, a.c - b.c}
}

// multiply expression by a constant
func mulExpr(a expr, c float64) expr {
    return expr{a.x * c, a.y * c, a.z * c, a.c * c}
}

// given a row of expressions, produce the next row up, by the given
// sum relation between blocks
func addRow(l []expr) []expr {
    if len(l) == 0 {
        panic("wrong")
    }
    r := make([]expr, len(l)-1)
    for i := range r {
        r[i] = addExpr(l[i], l[i+1])
    }
    return r
}

// given expression b in a variable, and expression a,
// take b == 0 and substitute to remove that variable from a.
func substX(a, b expr) expr {
    if b.x == 0 {
        panic("wrong")
    }
    return subExpr(a, mulExpr(b, a.x/b.x))
}

func substY(a, b expr) expr {
    if b.y == 0 {
        panic("wrong")
    }
    return subExpr(a, mulExpr(b, a.y/b.y))
}

func substZ(a, b expr) expr {
    if b.z == 0 {
        panic("wrong")
    }
    return subExpr(a, mulExpr(b, a.z/b.z))
}

// given an expression in a single variable, return value of that variable
func solveX(a expr) float64 {
    if a.x == 0 || a.y != 0 || a.z != 0 {
        panic("wrong")
    }
    return -a.c / a.x
}

func solveY(a expr) float64 {
    if a.x != 0 || a.y == 0 || a.z != 0 {
        panic("wrong")
    }
    return -a.c / a.y
}

func solveZ(a expr) float64 {
    if a.x != 0 || a.y != 0 || a.z == 0 {
        panic("wrong")
    }
    return -a.c / a.z
}

func main() {
    // representation of given information for bottom row
    r5 := []expr{{x: 1}, {c: 11}, {y: 1}, {c: 4}, {z: 1}}
    fmt.Println("bottom row:", r5)

    // given definition of brick sum relation
    r4 := addRow(r5)
    fmt.Println("next row up:", r4)
    r3 := addRow(r4)
    fmt.Println("middle row:", r3)

    // given relation y = x + z
    xyz := subExpr(expr{y: 1}, expr{x: 1, z: 1})
    fmt.Println("xyz relation:", xyz)
    // remove z from third cell using xyz relation
    r3[2] = substZ(r3[2], xyz)
    fmt.Println("middle row after substituting for z:", r3)

    // given cell = 40,
    b := expr{c: 40}
    // this gives an xy relation
    xy := subExpr(r3[0], b)
    fmt.Println("xy relation:", xy)
    // substitute 40 for cell
    r3[0] = b

    // remove x from third cell using xy relation
    r3[2] = substX(r3[2], xy)
    fmt.Println("middle row after substituting for x:", r3)

    // continue applying brick sum relation to get top cell
    r2 := addRow(r3)
    fmt.Println("next row up:", r2)
    r1 := addRow(r2)
    fmt.Println("top row:", r1)

    // given top cell = 151, we have an equation in y
    y := subExpr(r1[0], expr{c: 151})
    fmt.Println("y relation:", y)
    // using xy relation, we get an equation in x
    x := substY(xy, y)
    fmt.Println("x relation:", x)
    // using xyz relation, we get an equation in z
    z := substX(substY(xyz, y), x)
    fmt.Println("z relation:", z)

    // show final answers
    fmt.Println("x =", solveX(x))
    fmt.Println("y =", solveY(y))
    fmt.Println("z =", solveZ(z))
}
