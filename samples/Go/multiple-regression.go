package main

import (
    "code.google.com/p/gomatrix/matrix"
    "fmt"
)

func givens() (x, y *matrix.DenseMatrix) {
    height := []float64{1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63,
        1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83}
    weight := []float64{52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93,
        61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46}
    m := len(height)
    n := 3
    y = matrix.MakeDenseMatrix(weight, m, 1)
    x = matrix.Zeros(m, n)
    for i := 0; i < m; i++ {
        ip := float64(1)
        for j := 0; j < n; j++ {
            x.Set(i, j, ip)
            ip *= height[i]
        }
    }
    return
}

func main() {
    x, y := givens()
    n := x.Cols()
    q, r := x.QR()
    qty, err := q.Transpose().Times(y)
    if err != nil {
        fmt.Println(err)
        return
    }
    c := make([]float64, n)
    for i := n - 1; i >= 0; i-- {
        c[i] = qty.Get(i, 0)
        for j := i + 1; j < n; j++ {
            c[i] -= c[j] * r.Get(i, j)
        }
        c[i] /= r.Get(i, i)
    }
    fmt.Println(c)
}
