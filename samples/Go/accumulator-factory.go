package main

import "fmt"

func accumulator(sum interface{}) func(interface{}) interface{} {
    return func(nv interface{}) interface{} {
        switch s := sum.(type) {
        case int:
            switch n := nv.(type) {
            case int:
                sum = s + n
            case float64:
                sum = float64(s) + n
            }
        case float64:
            switch n := nv.(type) {
            case int:
                sum = s + float64(n)
            case float64:
                sum = s + n
            }
        default:
            sum = nv
        }
        return sum
    }
}

func main() {
    x := accumulator(1)
    x(5)
    accumulator(3)
    fmt.Println(x(2.3))
}
