package main

import "fmt"

func main() {
    var sum float32
    for _, x := range []float32{1,2,.5} {
        sum += x*x
    }
    fmt.Println(sum)
}
