package main

import (
	"fmt"
	"math/big"
)

var (
	ZERO = big.NewInt(0)
	ONE  = big.NewInt(1)
)

func Primes(n *big.Int) []*big.Int {
	res := []*big.Int{}
	mod, div := new(big.Int), new(big.Int)
	for i := big.NewInt(2); i.Cmp(n) != 1; {
		div.DivMod(n, i, mod)
		for mod.Cmp(ZERO) == 0 {
			res = append(res, new(big.Int).Set(i))
			n.Set(div)
			div.DivMod(n, i, mod)
		}
		i.Add(i, ONE)
	}
	return res
}

func main() {
	vals := []int64{
		1 << 31,
		1234567,
		333333,
		987653,
		2 * 3 * 5 * 7 * 11 * 13 * 17,
	}
	for _, v := range vals {
		fmt.Println(v, "->", Primes(big.NewInt(v)))
	}
}
