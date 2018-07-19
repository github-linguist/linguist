package main

import (
    "fmt"
    "os"
    "strconv"
)

func main() {
    if len(os.Args) != 2 {
        fmt.Println("Usage: k <Kelvin>")
        return
    }
    k, err := strconv.ParseFloat(os.Args[1], 64)
    if err != nil {
        fmt.Println(err)
        return
    }
    if k < 0 {
        fmt.Println("Kelvin must be >= 0.")
        return
    }
    fmt.Printf("K  %.2f\n", k)
    fmt.Printf("C  %.2f\n", k-273.15)
    fmt.Printf("F  %.2f\n", k*9/5-459.67)
    fmt.Printf("R  %.2f\n", k*9/5)
}
