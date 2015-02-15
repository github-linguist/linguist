package main

import "time"
import "fmt"

func main() {
    fmt.Println(time.Now().Format("2006-01-02"))
    fmt.Println(time.Now().Format("Monday, January 2, 2006"))
}
