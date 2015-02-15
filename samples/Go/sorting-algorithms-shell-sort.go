package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}

func main() {
    fmt.Println("before:", a)
    for inc := len(a) / 2; inc > 0; inc = (inc + 1) * 5 / 11 {
        for i := inc; i < len(a); i++ {
            j, temp := i, a[i]
            for ; j >= inc && a[j-inc] > temp; j -= inc {
                a[j] = a[j-inc]
            }
            a[j] = temp
        }
    }
    fmt.Println("after: ", a)
}
