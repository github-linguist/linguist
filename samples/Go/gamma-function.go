package main

import (
    "fmt"
    "math"
)

func main() {
    fmt.Println("    x               math.Gamma                 Lanczos7")
    for _, x := range []float64{-.5, .1, .5, 1, 1.5, 2, 3, 10, 140, 170} {
        fmt.Printf("%5.1f %24.16g %24.16g\n", x, math.Gamma(x), lanczos7(x))
    }
}

func lanczos7(z float64) float64 {
    t := z + 6.5
    x := .99999999999980993 +
        676.5203681218851/z -
        1259.1392167224028/(z+1) +
        771.32342877765313/(z+2) -
        176.61502916214059/(z+3) +
        12.507343278686905/(z+4) -
        .13857109526572012/(z+5) +
        9.9843695780195716e-6/(z+6) +
        1.5056327351493116e-7/(z+7)
    return math.Sqrt2 * math.SqrtPi * math.Pow(t, z-.5) * math.Exp(-t) * x
}
