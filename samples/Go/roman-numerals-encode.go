package main

import "fmt"

var (
    m0 = []string{"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"}
    m1 = []string{"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"}
    m2 = []string{"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"}
    m3 = []string{"", "M", "MM", "MMM", "I̅V̅",
        "V̅", "V̅I̅", "V̅I̅I̅", "V̅I̅I̅I̅", "I̅X̅"}
    m4 = []string{"", "X̅", "X̅X̅", "X̅X̅X̅", "X̅L̅",
        "L̅", "L̅X̅", "L̅X̅X̅", "L̅X̅X̅X̅", "X̅C̅"}
    m5 = []string{"", "C̅", "C̅C̅", "C̅C̅C̅", "C̅D̅",
        "D̅", "D̅C̅", "D̅C̅C̅", "D̅C̅C̅C̅", "C̅M̅"}
    m6 = []string{"", "M̅", "M̅M̅", "M̅M̅M̅"}
)

func formatRoman(n int) (string, bool) {
    if n < 1 || n >= 4e6 {
        return "", false
    }
    // this is efficient in Go.  the seven operands are evaluated,
    // then a single allocation is made of the exact size needed for the result.
    return m6[n/1e6] + m5[n%1e6/1e5] + m4[n%1e5/1e4] + m3[n%1e4/1e3] +
        m2[n%1e3/1e2] + m1[n%100/10] + m0[n%10],
        true
}

func main() {
    // show three numbers mentioned in task descriptions
    for _, n := range []int{1990, 2008, 1666} {
        r, ok := formatRoman(n)
        if ok {
            fmt.Println(n, "==", r)
        } else {
            fmt.Println(n, "not representable")
        }
    }
}
