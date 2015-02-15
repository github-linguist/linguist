package main

import (
    "fmt"
    "math"
)

func main() {
    // compute "extreme values" from non-extreme values
    var zero float64                         // zero is handy.
    var negZero, posInf, negInf, nan float64 // values to compute.
    negZero = zero * -1
    posInf = 1 / zero
    negInf = -1 / zero
    nan = zero / zero

    // print extreme values stored in variables
    fmt.Println(negZero, posInf, negInf, nan)

    // directly obtain extreme values
    fmt.Println(math.Float64frombits(1<<63),
        math.Inf(1), math.Inf(-1), math.NaN())

    // validate some arithmetic on extreme values
    fmt.Println()
    validateNaN(negInf+posInf, "-Inf + Inf")
    validateNaN(0*posInf, "0 * Inf")
    validateNaN(posInf/posInf, "Inf / Inf")
    // mod is specifically named in "What every computer scientist..."
    // Go math package doc lists many special cases for other package functions.
    validateNaN(math.Mod(posInf, 1), "Inf % 1")
    validateNaN(1+nan, "1 + NaN")
    validateZero(1/posInf, "1 / Inf")
    validateGT(posInf, math.MaxFloat64, "Inf > max value")
    validateGT(-math.MaxFloat64, negInf, "-Inf < max neg value")
    validateNE(nan, nan, "NaN != NaN")
    validateEQ(negZero, 0, "-0 == 0")
}

func validateNaN(n float64, op string) {
    if math.IsNaN(n) {
        fmt.Println(op, "-> NaN")
    } else {
        fmt.Println("!!! Expected NaN from", op, "  Found", n)
    }
}

func validateZero(n float64, op string) {
    if n == 0 {
        fmt.Println(op, "-> 0")
    } else {
        fmt.Println("!!! Expected 0 from", op, "  Found", n)
    }
}

func validateGT(a, b float64, op string) {
    if a > b {
        fmt.Println(op)
    } else {
        fmt.Println("!!! Expected", op, "  Found not true.")
    }
}

func validateNE(a, b float64, op string) {
    if a == b {
        fmt.Println("!!! Expected", op, "  Found not true.")
    } else {
        fmt.Println(op)
    }
}

func validateEQ(a, b float64, op string) {
    if a == b {
        fmt.Println(op)
    } else {
        fmt.Println("!!! Expected", op, "  Found not true.")
    }
}
