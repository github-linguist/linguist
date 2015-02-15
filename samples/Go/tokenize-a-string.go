package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "Hello,How,Are,You,Today"
    fmt.Println(strings.Join(strings.Split(s, ","), "."))
}
