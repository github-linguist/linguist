package main

import (
    "fmt"
    "math/big"
)

func main() {
    test(-2)
    test(-1)
    test(0)
    test(1)
    test(2)
    testBig("-222222222222222222222222222222222222")
    testBig("-1")
    testBig("0")
    testBig("1")
    testBig("222222222222222222222222222222222222")
}

func test(n int) {
    fmt.Printf("Testing integer %3d:  ", n)
    // & 1 is a good way to test
    if n&1 == 0 {
        fmt.Print("even ")
    } else {
        fmt.Print(" odd ")
    }
    // Careful when using %: negative n % 2 returns -1.  So, the code below
    // works, but can be broken by someone thinking they can reverse the
    // test by testing n % 2 == 1.  The valid reverse test is n % 2 != 0.
    if n%2 == 0 {
        fmt.Println("even")
    } else {
        fmt.Println(" odd")
    }
}

func testBig(s string) {
    b, _ := new(big.Int).SetString(s, 10)
    fmt.Printf("Testing big integer %v:  ", b)
    // the Bit function is the only sensible test for big ints.
    if b.Bit(0) == 0 {
        fmt.Println("even")
    } else {
        fmt.Println("odd")
    }
}
