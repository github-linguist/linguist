package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}

func main() {
    fmt.Println("before:", a)
    stoogesort(a)
    fmt.Println("after: ", a)
    fmt.Println("nyuk nyuk nyuk")
}

func stoogesort(a []int) {
    last := len(a) - 1
    if a[last] < a[0] {
        a[0], a[last] = a[last], a[0]
    }
    if last > 1 {
        t := len(a) / 3
        stoogesort(a[:len(a)-t])
        stoogesort(a[t:])
        stoogesort(a[:len(a)-t])
    }
}
