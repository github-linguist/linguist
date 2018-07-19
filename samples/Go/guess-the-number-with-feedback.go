package main

import (
    "fmt"
    "math/rand"
    "time"
)

const lower, upper = 1, 100

func main() {
    fmt.Printf("Guess integer number from %d to %d: ", lower, upper)
    rand.Seed(time.Now().Unix())
    n := rand.Intn(upper-lower+1) + lower
    for guess := n; ; {
        switch _, err := fmt.Scan(&guess); {
        case err != nil:
            fmt.Println("\n", err, "So, bye.")
            return
        case guess < n:
            fmt.Print("Too low. Try again: ")
        case guess > n:
            fmt.Print("Too high. Try again: ")
        default:
            fmt.Println("Well guessed!")
            return
        }
    }
}
