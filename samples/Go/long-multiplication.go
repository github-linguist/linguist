// Long multiplication per WP article referenced by task description.
// That is, multiplicand is multiplied by single digits of multiplier
// to form intermediate results.  Intermediate results are accumulated
// for the product.  Used here is the abacus method mentioned by the
// article, of summing intermediate results as they are produced,
// rather than all at once at the end.
//
// Limitations:  Negative numbers not supported, superfluous leading zeros
// not generally removed.
package main

import "fmt"

// argument validation
func d(b byte) byte {
    if b < '0' || b > '9' {
        panic("digit 0-9 expected")
    }
    return b - '0'
}

// add two numbers as strings
func add(x, y string) string {
    if len(y) > len(x) {
        x, y = y, x
    }
    b := make([]byte, len(x)+1)
    var c byte
    for i := 1; i <= len(x); i++ {
        if i <= len(y) {
            c += d(y[len(y)-i])
        }
        s := d(x[len(x)-i]) + c
        c = s / 10
        b[len(b)-i] = (s % 10) + '0'
    }
    if c == 0 {
        return string(b[1:])
    }
    b[0] = c + '0'
    return string(b)
}

// multipy a number by a single digit
func mulDigit(x string, y byte) string {
    if y == '0' {
        return "0"
    }
    y = d(y)
    b := make([]byte, len(x)+1)
    var c byte
    for i := 1; i <= len(x); i++ {
        s := d(x[len(x)-i])*y + c
        c = s / 10
        b[len(b)-i] = (s % 10) + '0'
    }
    if c == 0 {
        return string(b[1:])
    }
    b[0] = c + '0'
    return string(b)
}

// multiply two numbers as strings
func mul(x, y string) string {
    result := mulDigit(x, y[len(y)-1])
    for i, zeros := 2, ""; i <= len(y); i++ {
        zeros += "0"
        result = add(result, mulDigit(x, y[len(y)-i])+zeros)
    }
    return result
}

// requested output
const n = "18446744073709551616"

func main() {
    fmt.Println(mul(n, n))
}
