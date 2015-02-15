package main

import "fmt"

func main() {
    var a []interface{}
    a = append(a, 3)
    a = append(a, "apples", "oranges")
    fmt.Println(a)
}
