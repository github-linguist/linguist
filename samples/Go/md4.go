package main

import (
    "code.google.com/p/go.crypto/md4"
    "fmt"
)

func main() {
    h := md4.New()
    h.Write([]byte("Rosetta Code"))
    fmt.Printf("%x\n", h.Sum(nil))
}
