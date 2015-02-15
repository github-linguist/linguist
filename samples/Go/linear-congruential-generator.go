package main

import "fmt"

// basic linear congruential generator
func lcg(a, c, m, seed uint32) func() uint32 {
    r := seed
    return func() uint32 {
        r = (a*r + c) % m
        return r
    }
}

// microsoft generator has extra division step
func msg(seed uint32) func() uint32 {
    g := lcg(214013, 2531011, 1<<31, seed)
    return func() uint32 {
        return g() / (1 << 16)
    }
}

func example(seed uint32) {
    fmt.Printf("\nWith seed = %d\n", seed)
    bsd := lcg(1103515245, 12345, 1<<31, seed)
    msf := msg(seed)
    fmt.Println("       BSD  Microsoft")
    for i := 0; i < 5; i++ {
        fmt.Printf("%10d    %5d\n", bsd(), msf())
    }
}

func main() {
    example(0)
    example(1)
}
