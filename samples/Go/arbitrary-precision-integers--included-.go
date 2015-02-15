package main

import (
    "fmt"
    "math/big"
)

func main() {
    answer := big.NewInt(42)
    answer.Exp(big.NewInt(5), answer.Exp(big.NewInt(4),
        answer.Exp(big.NewInt(3), big.NewInt(2), nil), nil), nil)
    answer_string := answer.String()
    length := len(answer_string)
    fmt.Printf("has %d digits: %s ... %s\n",
        length,
        answer_string[0:20],
        answer_string[length-20:])
}
