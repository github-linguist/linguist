package main

import "fmt"
import "os"

func printFileSize(f string) {
    if stat, err := os.Stat(f); err != nil {
        fmt.Println(err)
    } else {
        fmt.Println(stat.Size())
    }
}

func main() {
    printFileSize("input.txt")
    printFileSize("/input.txt")
}
