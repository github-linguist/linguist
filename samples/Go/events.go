package main

import (
    "log"
    "os"
    "time"
)

func main() {
    l := log.New(os.Stdout, "", log.Ltime | log.Lmicroseconds)
    l.Println("program start")
    event := make(chan int)
    go func() {
        l.Println("task start")
        <-event
        l.Println("event reset by task")
    }()
    l.Println("program sleeping")
    time.Sleep(1 * time.Second)
    l.Println("program signaling event")
    event <- 0
    time.Sleep(100 * time.Millisecond)
}
