package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
    "time"
)

// The path to the lock file should be an absolute path starting from the root.
// (If you wish to prevent the same program running in different directories, that is.)
const lfn = "/tmp/rclock"

func main() {
    lf, err := os.OpenFile(lfn, os.O_RDWR|os.O_CREATE|os.O_EXCL, 0666)
    if err == nil {
        // good
        // 10 digit pid seems to be a standard for lock files
        fmt.Fprintf(lf, "%10d", os.Getpid())
        lf.Close()
        defer os.Remove(lfn)
    } else {
        // problem
        fmt.Println(err)
        // dig deeper
        lf, err = os.Open(lfn)
        if err != nil {
            return
        }
        defer lf.Close()
        fmt.Println("inspecting lock file...")
        b10 := make([]byte, 10)
        _, err = lf.Read(b10)
        if err != nil {
            fmt.Println(err)
            return
        }
        pid, err := strconv.Atoi(strings.TrimSpace(string(b10)))
        if err != nil {
            fmt.Println(err)
            return
        }
        fmt.Println("lock file created by pid", pid)
        return
    }
    fmt.Println(os.Getpid(), "running...")
    time.Sleep(1e10)
}
