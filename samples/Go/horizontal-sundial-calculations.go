package main

import (
    "fmt"
    "math"
    "os"
)

func getnum(prompt string) (r float64) {
    fmt.Print(prompt)
    if _, err := fmt.Scan(&r); err != nil {
        fmt.Println(err)
        os.Exit(-1)
    }
    return
}

func main() {
    lat := getnum("Enter latitude       => ")
    lng := getnum("Enter longitude      => ")
    ref := getnum("Enter legal meridian => ")
    slat := math.Sin(lat * math.Pi / 180)
    diff := lng - ref
    fmt.Println("\n    sine of latitude:   ", slat)
    fmt.Println("    diff longitude:     ", diff)
    fmt.Println("\nHour, sun hour angle, dial hour line angle from 6am to 6pm")
    for h := -6.; h <= 6; h++ {
        hra := 15*h - diff
        s, c := math.Sincos(hra * math.Pi / 180)
        hla := math.Atan2(slat*s, c) * 180 / math.Pi
        fmt.Printf("%2.0f %8.3f %8.3f\n", h, hra, hla)
    }
}
