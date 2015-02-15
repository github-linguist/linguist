package main

import (
    "fmt"
    "io"
    "os"
    "strings"
    "time"
)

const fn = "NOTES.TXT"

func main() {
    if len(os.Args) == 1 {
        f, err := os.Open(fn)
        if err != nil {
            // don't report "file does not exist" as an error, but
            // if it seems to be there but just can't be opened for
            // some reason, print original error from open attempt.
            if _, statErr := os.Stat(fn); statErr == nil {
                fmt.Println(err)
            }
            return
        }
        if _, err = io.Copy(os.Stdout, f); err != nil {
            fmt.Println(err)
        }
        if cErr := f.Close(); err == nil && cErr != nil {
            fmt.Println(err)
        }
        return
    }
    f, err := os.OpenFile(fn, os.O_RDWR|os.O_APPEND|os.O_CREATE, 0666)
    if err != nil {
        fmt.Println(err)
        return
    }
    _, err = fmt.Fprint(f, time.Now().Format(time.RFC1123),
        "\n\t", strings.Join(os.Args[1:], " "), "\n")
    if err != nil {
        fmt.Println(err)
    }
    if cErr := f.Close(); err == nil && cErr != nil {
        fmt.Println(cErr)
    }
}
