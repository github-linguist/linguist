package main

import "fmt"

func main() {
    sieve(1e6)
    if !search(6, 1e6, "left", func(n, pot int) int { return n % pot }) {
        panic("997?")
    }
    if !search(6, 1e6, "right", func(n, _ int) int { return n / 10 }) {
        panic("7393?")
    }
}

var c []bool

func sieve(ss int) {
    c = make([]bool, ss)
    c[1] = true
    for p := 2; ; {
        p2 := p * p
        if p2 >= ss {
            break
        }
        for i := p2; i < ss; i += p {
            c[i] = true
        }
        for {
            p++
            if !c[p] {
                break
            }
        }
    }
}

func search(digits, pot int, s string, truncFunc func(n, pot int) int) bool {
    n := pot - 1
    pot /= 10
smaller:
    for ; n >= pot; n -= 2 {
        for tn, tp := n, pot; tp > 0; tp /= 10 {
            if tn < tp || c[tn] {
                continue smaller
            }
            tn = truncFunc(tn, tp)
        }
        fmt.Println("max", s, "truncatable:", n)
        return true
    }
    if digits > 1 {
        return search(digits-1, pot, s, truncFunc)
    }
    return false
}
