package main

import "fmt"

func main() {
    for value := 0;; {
        value++
        fmt.Println(value)
        if value % 6 == 0 {
            break
        }
    }
}
