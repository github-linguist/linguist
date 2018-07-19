package main

import (
    "fmt"
    "math"
)

// fdy is a type for function f used in Euler's method.
type fdy func(float64, float64) float64

// eulerStep computes a single new value using Euler's method.
// Note that step size h is a parameter, so a variable step size
// could be used.
func eulerStep(f fdy, x, y, h float64) float64 {
    return y + h*f(x, y)
}

// Definition of cooling rate.  Note that this has general utility and
// is not specific to use in Euler's method.

// newCoolingRate returns a function that computes cooling rate
// for a given cooling rate constant k.
func newCoolingRate(k float64) func(float64) float64 {
    return func(deltaTemp float64) float64 {
        return -k * deltaTemp
    }
}

// newTempFunc returns a function that computes the analytical solution
// of cooling rate integrated over time.
func newTempFunc(k, ambientTemp, initialTemp float64) func(float64) float64 {
    return func(time float64) float64 {
        return ambientTemp + (initialTemp-ambientTemp)*math.Exp(-k*time)
    }
}

// newCoolingRateDy returns a function of the kind needed for Euler's method.
// That is, a function representing dy(x, y(x)).
//
// Parameters to newCoolingRateDy are cooling constant k and ambient
// temperature.
func newCoolingRateDy(k, ambientTemp float64) fdy {
    crf := newCoolingRate(k)
    // note that result is dependent only on the object temperature.
    // there are no additional dependencies on time, so the x parameter
    // provided by eulerStep is unused.
    return func(_, objectTemp float64) float64 {
        return crf(objectTemp - ambientTemp)
    }
}

func main() {
    k := .07
    tempRoom := 20.
    tempObject := 100.
    fcr := newCoolingRateDy(k, tempRoom)
    analytic := newTempFunc(k, tempRoom, tempObject)
    for _, deltaTime := range []float64{2, 5, 10} {
        fmt.Printf("Step size = %.1f\n", deltaTime)
        fmt.Println(" Time Euler's Analytic")
        temp := tempObject
        for time := 0.; time <= 100; time += deltaTime {
            fmt.Printf("%5.1f %7.3f %7.3f\n", time, temp, analytic(time))
            temp = eulerStep(fcr, time, temp, deltaTime)
        }
        fmt.Println()
    }
}
