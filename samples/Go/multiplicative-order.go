package main

import (
    "fmt"
    "math/big"
)

func main() {
    moTest(big.NewInt(37), big.NewInt(3343))
    b := big.NewInt(100)
    moTest(b.Add(b.Exp(ten, b, nil), one), big.NewInt(7919))
    moTest(b.Add(b.Exp(ten, b.SetInt64(1000), nil), one), big.NewInt(15485863))
    moTest(b.Sub(b.Exp(ten, b.SetInt64(10000), nil), one),
        big.NewInt(22801763489))

    moTest(big.NewInt(1511678068), big.NewInt(7379191741))
    moTest(big.NewInt(3047753288), big.NewInt(2257683301))
}

func moTest(a, n *big.Int) {
    if a.BitLen() < 100 {
        fmt.Printf("ord(%v)", a)
    } else {
        fmt.Print("ord([big])")
    }
    if n.BitLen() < 100 {
        fmt.Printf(" mod %v ", n)
    } else {
        fmt.Print(" mod [big] ")
    }
    if !n.ProbablyPrime(20) {
        fmt.Println("not computed.  modulus must be prime for this algorithm.")
        return
    }
    fmt.Println("=", moBachShallit58(a, n, factor(new(big.Int).Sub(n, one))))
}

var one = big.NewInt(1)
var two = big.NewInt(2)
var ten = big.NewInt(10)

func moBachShallit58(a, n *big.Int, pf []pExp) *big.Int {
    n1 := new(big.Int).Sub(n, one)
    var x, y, o1, g big.Int
    mo := big.NewInt(1)
    for _, pe := range pf {
        y.Quo(n1, y.Exp(pe.prime, big.NewInt(pe.exp), nil))
        var o int64
        for x.Exp(a, &y, n); x.Cmp(one) > 0; o++ {
            x.Exp(&x, pe.prime, n)
        }
        o1.Exp(pe.prime, o1.SetInt64(o), nil)
        mo.Mul(mo, o1.Quo(&o1, g.GCD(nil, nil, mo, &o1)))
    }
    return mo
}

type pExp struct {
    prime *big.Int
    exp   int64
}

func factor(n *big.Int) (pf []pExp) {
    var e int64
    for ; n.Bit(int(e)) == 0; e++ {
    }
    if e > 0 {
        n.Rsh(n, uint(e))
        pf = []pExp{{big.NewInt(2), e}}
    }
    s := sqrt(n)
    q, r := new(big.Int), new(big.Int)
    for d := big.NewInt(3); n.Cmp(one) > 0; d.Add(d, two) {
        if d.Cmp(s) > 0 {
            d.Set(n)
        }
        for e = 0; ; e++ {
            q.QuoRem(n, d, r)
            if r.BitLen() > 0 {
                break
            }
            n.Set(q)
        }
        if e > 0 {
            pf = append(pf, pExp{new(big.Int).Set(d), e})
            s = sqrt(n)
        }
    }
    return
}

func sqrt(n *big.Int) *big.Int {
    a := new(big.Int)
    for b := new(big.Int).Set(n); ; {
        a.Set(b)
        b.Rsh(b.Add(b.Quo(n, a), a), 1)
        if b.Cmp(a) >= 0 {
            return a
        }
    }
    return a.SetInt64(0)
}
