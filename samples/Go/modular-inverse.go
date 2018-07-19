package main

import (
	"fmt"
	"math/big"
)

func main() {
	a := big.NewInt(42)
	m := big.NewInt(2017)
	k := new(big.Int).ModInverse(a, m)
	fmt.Println(k)
}
