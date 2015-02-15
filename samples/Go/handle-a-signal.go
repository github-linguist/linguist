package main

import (
    "fmt"
    "os"
    "os/signal"
    "time"
)

func main() {
    start := time.Now()
    k := time.Tick(time.Second / 2)
    sc := make(chan os.Signal, 1)
    signal.Notify(sc, os.Interrupt)
    for n := 1; ; {
        // not busy waiting, this blocks until one of the two
        // channel operations is possible
        select {
        case <-k:
            fmt.Println(n)
            n++
        case <-sc:
            fmt.Printf("Ran for %f seconds.\n",
                time.Now().Sub(start).Seconds())
            return
        }
    }
}
