package main

import (
    "fmt"
    "math/big"
)

// collection of numbers.  A slice is used for the collection.
// The elements are big integers, since that's what the function Primes
// uses (as was specified by the Prime decomposition task.)
var numbers = []*big.Int{
    big.NewInt(12757923),
    big.NewInt(12878611),
    big.NewInt(12878893),
    big.NewInt(12757923),
    big.NewInt(15808973),
    big.NewInt(15780709),
}

// main just calls the function specified by the task description and
// prints results.  note it allows for multiple numbers with the largest
// minimal factor.  the task didn't specify to handle this, but obviously
// it's possible.
func main() {
    rs := lmf(numbers)
    fmt.Println("largest minimal factor:", rs[0].decomp[0])
    for _, r := range rs {
        fmt.Println(r.number, "->", r.decomp)
    }
}

// this type associates a number with it's prime decomposition.
// the type is neccessary so that they can be sent together over
// a Go channel, but it turns out to be convenient as well for
// the return type of lmf.
type result struct {
    number *big.Int
    decomp []*big.Int
}

// the function specified by the task description, "largest minimal factor."
func lmf([]*big.Int) []result {
    // construct result channel and start a goroutine to decompose each number.
    // goroutines run in parallel as CPU cores are available.
    rCh := make(chan result)
    for _, n := range numbers {
        go decomp(n, rCh)
    }

    // collect results.  <-rCh returns a single result from the result channel.
    // we know how many results to expect so code here collects exactly that
    // many results, and accumulates a list of those with the largest
    // minimal factor.
    rs := []result{<-rCh}
    for i := 1; i < len(numbers); i++ {
        switch r := <-rCh; r.decomp[0].Cmp(rs[0].decomp[0]) {
        case 1:
            rs = rs[:1]
            rs[0] = r
        case 0:
            rs = append(rs, r)
        }
    }
    return rs
}

// decomp is the function run as a goroutine.  multiple instances of this
// function will run concurrently, one for each number being decomposed.
// it acts as a driver for Primes, calling Primes as needed, packaging
// the result, and sending the packaged result on the channel.
// "as needed" turns out to mean sending Primes a copy of n, as Primes
// as written is destructive on its argument.
func decomp(n *big.Int, rCh chan result) {
    rCh <- result{n, Primes(new(big.Int).Set(n))}
}

// code below copied from Prime decomposition task
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
