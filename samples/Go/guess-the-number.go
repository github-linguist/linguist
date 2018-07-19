package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    fmt.Print("Guess number from 1 to 10: ")
    rand.Seed(time.Now().Unix())
    n := rand.Intn(10) + 1
    for guess := n; ; fmt.Print("No. Try again: ") {
        switch _, err := fmt.Scan(&guess); {
        case err != nil:
            fmt.Println("\n", err, "\nSo, bye.")
            return
        case guess == n:
            fmt.Println("Well guessed!")
            return
        }
    }
}
