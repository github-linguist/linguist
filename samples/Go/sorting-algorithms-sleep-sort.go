package main

import (
    "log"
    "os"
    "strconv"
    "sync"
    "time"
)

func main() {
    lg := log.New(os.Stdout, "", 0)
    var wg sync.WaitGroup
    wg.Add(len(os.Args) - 1)
    for _, a := range os.Args[1:] {
        if i, err := strconv.ParseInt(a, 10, 64); err != nil {
            lg.Print(err)
            wg.Done()
        } else {
            time.AfterFunc(time.Duration(i*int64(time.Second)), func() {
                lg.Print(i)
                wg.Done()
            })
        }
    }
    wg.Wait()
}
