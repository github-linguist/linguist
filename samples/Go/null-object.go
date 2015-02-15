package main

import "fmt"

var (
    s []int       // slice type
    p *int        // pointer type
    f func()      // function type
    i interface{} // interface type
    m map[int]int // map type
    c chan int    // channel type
)

func main() {
    fmt.Println(s == nil)
    fmt.Println(p == nil)
    fmt.Println(f == nil)
    fmt.Println(i == nil)
    fmt.Println(m == nil)
    fmt.Println(c == nil)
}
