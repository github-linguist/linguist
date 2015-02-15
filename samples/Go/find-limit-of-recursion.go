package main

import "fmt"

func main() {
    r(1)
}

func r(l int) {
    if l % 1000 == 0 {
        fmt.Println(l)
    }
    r(l+1)
}
