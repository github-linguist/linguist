package main

import "fmt"

var a1 = []int{'a','b','c'}
var a2 = []int{'A','B','C'}
var a3 = []int{1,2,3}

func main() {
    for i := range a1 {
        fmt.Printf("%c%c%d\n", a1[i], a2[i], a3[i])
    }
}
