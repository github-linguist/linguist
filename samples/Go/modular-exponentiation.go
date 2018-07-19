package main

import (
    "fmt"
    "math/big"
)

func main() {
    a, _ := new(big.Int).SetString(
        "2988348162058574136915891421498819466320163312926952423791023078876139", 10)
    b, _ := new(big.Int).SetString(
        "2351399303373464486466122544523690094744975233415544072992656881240319", 10)
    m := big.NewInt(10)
    r := big.NewInt(40)
    m.Exp(m, r, nil)

    r.Exp(a, b, m)
    fmt.Println(r)
}
