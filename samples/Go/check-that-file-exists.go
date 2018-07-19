package main

import (
    "fmt"
    "os"
)

func printStat(p string) {
    switch i, err := os.Stat(p); {
    case err != nil:
        fmt.Println(err)
    case i.IsDir():
        fmt.Println(p, "is a directory")
    default:
        fmt.Println(p, "is a file")
    }
}

func main() {
    printStat("input.txt")
    printStat("/input.txt")
    printStat("docs")
    printStat("/docs")
}
