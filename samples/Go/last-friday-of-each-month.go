package main

import (
    "fmt"
    "os"
    "strconv"
    "time"
)

func main() {
    y := time.Now().Year()
    if len(os.Args) == 2 {
        if i, err := strconv.Atoi(os.Args[1]); err == nil {
            y = i
        }
    }
    for m := time.January; m <= time.December; m++ {
        d := time.Date(y, m+1, 1, 0, 0, 0, 0, time.UTC).Add(-24 * time.Hour)
        d = d.Add(-time.Duration((d.Weekday() + 2) % 7) * 24 * time.Hour)
        fmt.Println(d.Format("2006-01-02"))
    }
}
