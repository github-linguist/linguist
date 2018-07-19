package main

import (
    "fmt"
    "sort"
    "sync"
    "time"
)

// data type for history variable (its an int)
type history struct {
    timestamp tsFunc
    hs        []hset
}

// data type for timestamp generator
type tsFunc func() time.Time

// data type for a "set" event
type hset struct {
    int           // new value
    t   time.Time // timestamp
}

// newHistory creates a history variable
func newHistory(ts tsFunc) history {
    return history{ts, []hset{{t: ts()}}}
}

// int returns the current value
func (h history) int() int {
    return h.hs[len(h.hs)-1].int
}

// set does what you expect and returns the timestamp recorded for the event
func (h *history) set(x int) time.Time {
    t := h.timestamp()
    h.hs = append(h.hs, hset{x, t})
    return t
}

// dump displays a complete history
func (h history) dump() {
    for _, hs := range h.hs {
        fmt.Println(hs.t.Format(time.StampNano), hs.int)
    }
}

// recall recalls the value stored in the history variable at time t.
// if the variable had not been created yet, ok is false.
func (h history) recall(t time.Time) (int, /*ok*/ bool) {
    i := sort.Search(len(h.hs), func(i int) bool {
        return h.hs[i].t.After(t)
    })
    if i > 0 {
        return h.hs[i-1].int, true
    }
    return 0, false
}

// newTimestamper returns a function that generates unique timestamps.
// Use a single timestamper for multiple history variables to preserve
// an unambiguous sequence of assignments across the multiple history
// variables within a single goroutine.
func newTimestamper() tsFunc {
    var last time.Time
    return func() time.Time {
        if t := time.Now(); t.After(last) {
            last = t
        } else {
            last.Add(1)
        }
        return last
    }
}

// newProtectedTimestamper generates unique timestamps for concurrent
// goroutines.
func newProtectedTimestamper() tsFunc {
    var last time.Time
    var m sync.Mutex
    return func() (t time.Time) {
        t = time.Now()
        m.Lock() // m protects last
        if t.After(last) {
            last = t
        } else {
            last.Add(1)
            t = last
        }
        m.Unlock()
        return
    }
}

func main() {
    // enable history variable support appropriate for single goroutine.
    ts := newTimestamper()
    // define a history variable
    h := newHistory(ts)
    // assign three values.  (timestamps kept for future reference.)
    ref := []time.Time{h.set(3), h.set(1), h.set(4)}
    // non-destructively display history
    fmt.Println("History of variable h:")
    h.dump()
    // recall the three values.  (this is non-destructive as well, but
    // different than the dump in that values are recalled by time.)
    fmt.Println("Recalling values:")
    for _, t := range ref {
        rv, _ := h.recall(t)
        fmt.Println(rv)
    }
}
