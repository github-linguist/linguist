package main

import (
    "fmt"
    "math"
    "time"
)

// type for input function, k.
// input is duration since an arbitrary start time t0.
type tFunc func(time.Duration) float64

// active integrator object.  state variables are not here, but in
// function aif, started as a goroutine in the constructor.
type aio struct {
    iCh chan tFunc        // channel for setting input function
    oCh chan chan float64 // channel for requesting output
}

// constructor
func newAio() *aio {
    var a aio
    a.iCh = make(chan tFunc)
    a.oCh = make(chan chan float64)
    go aif(&a)
    return &a
}

// input method required by task description.  in practice, this method is
// unnecessary; you would just put that single channel send statement in
// your code wherever you wanted to set the input function.
func (a aio) input(f tFunc) {
    a.iCh <- f
}

// output method required by task description.  in practice, this method too
// would not likely be best.  instead any client interested in the value would
// likely make a return channel sCh once, and then reuse it as needed.
func (a aio) output() float64 {
    sCh := make(chan float64)
    a.oCh <- sCh
    return <-sCh
}

// integration function that returns constant 0
func zeroFunc(time.Duration) float64 { return 0 }

// goroutine serializes access to integrated function k and state variable s
func aif(a *aio) {
    var k tFunc = zeroFunc // integration function
    s := 0.                // "object state" initialized to 0
    t0 := time.Now()       // initial time
    k0 := k(0)             // initial sample value
    t1 := t0               // t1, k1 used for trapezoid formula
    k1 := k0

    tk := time.Tick(10 * time.Millisecond) // 10 ms -> 100 Hz
    for {
        select {
        case t2 := <-tk: // timer tick event
            k2 := k(t2.Sub(t0))                        // new sample value
            s += (k1 + k2) * .5 * t2.Sub(t1).Seconds() // trapezoid formula
            t1, k1 = t2, k2                            // save time and value
        case k = <-a.iCh: // input method event: function change
        case sCh := <-a.oCh: // output method event: sample object state
            sCh <- s
        }
    }
}

func main() {
    a := newAio()                           // create object
    a.input(func(t time.Duration) float64 { // 1. set input to sin function
        return math.Sin(t.Seconds() * math.Pi)
    })
    time.Sleep(2 * time.Second) // 2. sleep 2 sec
    a.input(zeroFunc)           // 3. set input to zero function
    time.Sleep(time.Second / 2) // 4. sleep .5 sec
    fmt.Println(a.output())     // output should be near zero
}
