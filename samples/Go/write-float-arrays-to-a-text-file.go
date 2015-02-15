package main

import (
    "fmt"
    "os"
)

var (
    x = []float64{1, 2, 3, 1e11}
    y = []float64{1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791}

    xprecision = 3
    yprecision = 5
)

func main() {
    if len(x) != len(y) {
        fmt.Println("x, y different length")
        return
    }
    f, err := os.Create("filename")
    if err != nil {
        fmt.Println(err)
        return
    }
    for i := range x {
        fmt.Fprintf(f, "%.*e, %.*e\n", xprecision-1, x[i], yprecision-1, y[i])
    }
    f.Close()
}
