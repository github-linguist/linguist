package main

import (
    "fmt"
    "strconv"
)

func kaprekar(n uint64, base uint64) (bool, int) {
    order := 0
    if n == 1 {
        return true, -1
    }

    nn, power := n*n, uint64(1)
    for power <= nn {
        power *= base
        order++
    }

    power /= base
    order--
    for ; power > 1; power /= base {
        q, r := nn/power, nn%power
        if q >= n {
            return false, -1
        }

        if q+r == n {
            return true, order
        }

        order--
    }

    return false, -1
}

func main() {
    max := uint64(10000)
    fmt.Printf("Kaprekar numbers < %d:\n", max)
    for m := uint64(0); m < max; m++ {
        if is, _ := kaprekar(m, 10); is {
            fmt.Println("  ", m)
        }
    }

    // extra credit
    max = 1e6
    var count int
    for m := uint64(0); m < max; m++ {
        if is, _ := kaprekar(m, 10); is {
            count++
        }
    }
    fmt.Printf("\nThere are %d Kaprekar numbers < %d.\n", count, max)

    // extra extra credit
    const base = 17
    maxB := "1000000"
    fmt.Printf("\nKaprekar numbers between 1 and %s(base %d):\n", maxB, base)
    max, _ = strconv.ParseUint(maxB, base, 64)
    fmt.Printf("\n Base 10  Base %d        Square       Split\n", base)
    for m := uint64(2); m < max; m++ {
        is, pos := kaprekar(m, base)
        if !is {
            continue
        }
        sq := strconv.FormatUint(m*m, base)
        str := strconv.FormatUint(m, base)
        split := len(sq)-pos
        fmt.Printf("%8d  %7s  %12s  %6s + %s\n", m,
            str, sq, sq[:split], sq[split:]) // optional extra extra credit
    }
}
