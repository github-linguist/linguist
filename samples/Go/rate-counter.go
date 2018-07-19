package main

import (
    "fmt"
    "math/rand"
    "time"
)

// representation of time.Time is nanosecond, actual resolution system specific
type rateStateS struct {
    lastFlush time.Time
    period    time.Duration
    tickCount int
}

func ticRate(pRate *rateStateS) {
    pRate.tickCount++
    now := time.Now()
    if now.Sub(pRate.lastFlush) >= pRate.period {
        // TPS Report
        tps := 0.
        if pRate.tickCount > 0 {
            tps = float64(pRate.tickCount) / now.Sub(pRate.lastFlush).Seconds()
        }
        fmt.Println(tps, "tics per second.")

        // Reset
        pRate.tickCount = 0
        pRate.lastFlush = now
    }
}

func somethingWeDo() {
    time.Sleep(time.Duration(9e7 + rand.Int63n(2e7))) // sleep about .1 second.
}

func main() {
    start := time.Now()

    rateWatch := rateStateS{
        lastFlush: start,
        period:    5 * time.Second,
    }

    // Loop for twenty seconds
    latest := start
    for latest.Sub(start) < 20*time.Second {
        somethingWeDo()
        ticRate(&rateWatch)
        latest = time.Now()
    }
}
