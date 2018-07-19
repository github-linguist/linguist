package main

import "time"
import "fmt"

func main() {
    fmt.Print("Enter number of seconds to sleep: ")
    var sec float64
    fmt.Scanf("%f", &sec)
    fmt.Print("Sleeping…")
    time.Sleep(time.Duration(sec * float64(time.Second)))
    fmt.Println("\nAwake!")
}
