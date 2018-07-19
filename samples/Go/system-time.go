package main

import "time"
import "fmt"

func main() {
    t := time.Now()
    fmt.Println(t)                                    // default format
    fmt.Println(t.Format("Mon Jan  2 15:04:05 2006")) // some custom format
}
