package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Printf("PID: %v\n", os.Getpid())
    if len(os.Args) < 2 {
        fmt.Println("Done.")
        return
    }
    cp, err := os.StartProcess(os.Args[0], nil,
        &os.ProcAttr{Files: []*os.File{nil, os.Stdout}},
    )
    if err != nil {
        fmt.Println(err)
    }
    // Child process running independently at this point.
    // We have its PID and can print it.
    fmt.Printf("Child's PID: %v\n", cp.Pid)
    if _, err = cp.Wait(); err != nil {
        fmt.Println(err)
    }
}
