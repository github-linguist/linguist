package main

import (
    "fmt"
    "math"
    "math/big"
)

func main() {
    var recip big.Rat
    max := int64(1 << 19)
    for candidate := int64(2); candidate < max; candidate++ {
        sum := big.NewRat(1, candidate)
        max2 := int64(math.Sqrt(float64(candidate)))
        for factor := int64(2); factor <= max2; factor++ {
            if candidate%factor == 0 {
                sum.Add(sum, recip.SetFrac64(1, factor))
                if f2 := candidate / factor; f2 != factor {
                    sum.Add(sum, recip.SetFrac64(1, f2))
                }
            }
        }
        if sum.Denom().Int64() == 1 {
            perfectstring := ""
            if sum.Num().Int64() == 1 {
                perfectstring = "perfect!"
            }
            fmt.Printf("Sum of recipr. factors of %d = %d exactly %s\n",
                candidate, sum.Num().Int64(), perfectstring)
        }
    }
}
