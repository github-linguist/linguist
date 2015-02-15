package main

import "fmt"
import "math/rand"
import "time"

func main() {
    rand.Seed(time.Now().UnixNano())
    for {
        a := rand.Intn(20)
        fmt.Println(a)
        if a == 10 {
            break
        }
        b := rand.Intn(20)
        fmt.Println(b)
    }
}
