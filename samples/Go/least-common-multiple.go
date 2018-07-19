package main

import (
    "fmt"
    "math/big"
)

var m, n, z big.Int

func init() {
    m.SetString("2562047788015215500854906332309589561", 10)
    n.SetString("6795454494268282920431565661684282819", 10)
}

func main() {
    fmt.Println(z.Mul(z.Div(&m, z.GCD(nil, nil, &m, &n)), &n))
}
