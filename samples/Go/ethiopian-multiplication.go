package main

import "fmt"

func halve(i int) int { return i/2 }

func double(i int) int { return i*2 }

func isEven(i int) bool { return i%2 == 0 }

func ethMulti(i, j int) (r int) {
    for ; i > 0; i, j = halve(i), double(j) {
        if !isEven(i) {
            r += j
        }
    }
    return
}

func main() {
    fmt.Printf("17 ethiopian 34 = %d\n", ethMulti(17, 34))
}
