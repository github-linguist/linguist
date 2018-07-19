package main

import (
    "fmt"
    "math/big"
)

type mTerm struct {
    a, n, d int64
}

var testCases = [][]mTerm{
    {{1, 1, 2}, {1, 1, 3}},
    {{2, 1, 3}, {1, 1, 7}},
    {{4, 1, 5}, {-1, 1, 239}},
    {{5, 1, 7}, {2, 3, 79}},
    {{1, 1, 2}, {1, 1, 5}, {1, 1, 8}},
    {{4, 1, 5}, {-1, 1, 70}, {1, 1, 99}},
    {{5, 1, 7}, {4, 1, 53}, {2, 1, 4443}},
    {{6, 1, 8}, {2, 1, 57}, {1, 1, 239}},
    {{8, 1, 10}, {-1, 1, 239}, {-4, 1, 515}},
    {{12, 1, 18}, {8, 1, 57}, {-5, 1, 239}},
    {{16, 1, 21}, {3, 1, 239}, {4, 3, 1042}},
    {{22, 1, 28}, {2, 1, 443}, {-5, 1, 1393}, {-10, 1, 11018}},
    {{22, 1, 38}, {17, 7, 601}, {10, 7, 8149}},
    {{44, 1, 57}, {7, 1, 239}, {-12, 1, 682}, {24, 1, 12943}},
    {{88, 1, 172}, {51, 1, 239}, {32, 1, 682}, {44, 1, 5357}, {68, 1, 12943}},
    {{88, 1, 172}, {51, 1, 239}, {32, 1, 682}, {44, 1, 5357}, {68, 1, 12944}},
}

func main() {
    for _, m := range testCases {
        fmt.Printf("tan %v = %v\n", m, tans(m))
    }
}

var one = big.NewRat(1, 1)

func tans(m []mTerm) *big.Rat {
    if len(m) == 1 {
        return tanEval(m[0].a, big.NewRat(m[0].n, m[0].d))
    }
    half := len(m) / 2
    a := tans(m[:half])
    b := tans(m[half:])
    r := new(big.Rat)
    return r.Quo(new(big.Rat).Add(a, b), r.Sub(one, r.Mul(a, b)))
}

func tanEval(coef int64, f *big.Rat) *big.Rat {
    if coef == 1 {
        return f
    }
    if coef < 0 {
        r := tanEval(-coef, f)
        return r.Neg(r)
    }
    ca := coef / 2
    cb := coef - ca
    a := tanEval(ca, f)
    b := tanEval(cb, f)
    r := new(big.Rat)
    return r.Quo(new(big.Rat).Add(a, b), r.Sub(one, r.Mul(a, b)))
}
