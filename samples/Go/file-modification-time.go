package main

import (
    "fmt"
    "os"
    "syscall"
    "time"
)

var filename = "input.txt"

func main() {
    foo, err := os.Stat(filename)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("mod time was:", foo.ModTime())
    mtime := time.Now()
    atime := mtime // a default, because os.Chtimes has an atime parameter.
    // but see if there's a real atime that we can preserve.
    if ss, ok := foo.Sys().(*syscall.Stat_t); ok {
        atime = time.Unix(ss.Atim.Sec, ss.Atim.Nsec)
    }
    os.Chtimes(filename, atime, mtime)
    fmt.Println("mod time now:", mtime)
}
