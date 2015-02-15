package main

import (
    "fmt"
    "os"
)

func createFile(fn string) {
    // create new; don't overwrite an existing file.
    f, err := os.Create(fn)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("file", fn, "created!")
    f.Close()
}

func createDir(dn string) {
    err := os.Mkdir(dn, 0666)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("directory", dn, "created!")
}

func main() {
    createFile("input.txt")
    createFile("/input.txt")
    createDir("docs")
    createDir("/docs")
}
