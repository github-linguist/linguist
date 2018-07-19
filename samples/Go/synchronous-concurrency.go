package main

import (
    "fmt"
    "bufio"
    "io"
    "os"
)

// main, one of two goroutines used, will function as the "reading unit"
func main() {
    // get file open first
    f, err := os.Open("input.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer f.Close()
    lr := bufio.NewReader(f)

    // that went ok, now create communication channels,
    // and start second goroutine as the "printing unit"
    lines := make(chan string)
    count := make(chan int)
    go printer(lines, count)

    for {
        line, prefix, err := lr.ReadLine()
        switch {
        case err == io.EOF:
        case err != nil:
            fmt.Println(err)
        case prefix:
            fmt.Println("unexpected long line")
        default:
            lines <- string(line)
            continue
        }
        break
    }
    // this represents the request for the printer to send the count
    close(lines)
    // wait for the count from the printer, then print it, then exit
    fmt.Println("Number of lines:", <-count)
}

func printer(in <-chan string, count chan<- int) {
    c := 0
    // loop as long as in channel stays open
    for s := range in {
        fmt.Println(s)
        c++
    }
    // make count available on count channel, then return (terminate goroutine)
    count <- c
}
