package main

import "fmt"

func enc(b int) int {
    return b ^ b>>1
}

func dec(g int) (b int) {
    for ; g != 0; g >>= 1 {
        b ^= g
    }
    return
}

func main() {
    fmt.Println("decimal  binary   gray    decoded")
    for b := 0; b < 32; b++ {
        g := enc(b)
        d := dec(g)
        fmt.Printf("  %2d     %05b   %05b   %05b  %2d\n", b, b, g, d, d)
    }
}
