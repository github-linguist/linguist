package main

import (
    "crypto/rand"
    "encoding/binary"
    "fmt"
    "io"
    "os"
)

func main() {
    testRandom("crypto/rand", rand.Reader)
    testRandom("dev/random", newDevRandom())
}

func newDevRandom() (f *os.File) {
    var err error
    if f, err = os.Open("/dev/random"); err != nil {
        panic(err)
    }
    return
}

func testRandom(label string, src io.Reader) {
    fmt.Printf("%s:\n", label)
    var r int32
    for i := 0; i < 10; i++ {
        if err := binary.Read(src, binary.LittleEndian, &r); err != nil {
            panic(err)
        }
        fmt.Print(r, " ")
    }
    fmt.Println()
}
