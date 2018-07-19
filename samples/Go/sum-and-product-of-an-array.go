package main

import "fmt"

func main() {
    sum, prod := 0, 1
    for _, x := range []int{1,2,5} {
        sum += x
        prod *= x
    }
    fmt.Println(sum, prod)
}
