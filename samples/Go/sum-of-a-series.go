package main

import ("fmt"; "math")

func main() {
    fmt.Println("known:   ", math.Pi*math.Pi/6)
    sum := 0.
    for i := 1e3; i > 0; i-- {
        sum += 1 / (i * i)
    }
    fmt.Println("computed:", sum)
}
