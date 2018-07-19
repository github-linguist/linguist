package main

import "fmt"

func main() {
    a := []int{90, 47, 58, 29, 22, 32, 55, 5, 55, 73}
    fmt.Println(a)
    fmt.Println(fd(a, 9))
}

func fd(a []int, ord int) []int {
    for i := 0; i < ord; i++ {
        for j := 0; j < len(a)-i-1; j++ {
            a[j] = a[j+1] - a[j]
        }
    }
    return a[:len(a)-ord]
}
