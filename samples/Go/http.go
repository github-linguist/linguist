package main

import (
    "io"
    "log"
    "net/http"
    "os"
)

func main() {
    r, err := http.Get("http://rosettacode.org/robots.txt")
    if err != nil {
        log.Fatalln(err)
    }
    io.Copy(os.Stdout, r.Body)
}
