package main

import (
    "fmt"
    "math/rand"
    "time"
)

var list = []string{"bleen", "fuligin", "garrow", "grue", "hooloovoo"}

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println(list[rand.Intn(len(list))])
}
