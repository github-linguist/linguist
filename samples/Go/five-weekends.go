package main

import (
    "fmt"
    "time"
)

func main() {
    var n int                                 // for task item 2
    var first, last time.Time                 // for task item 3
    haveNone := make([]int, 0, 29)            // for extra credit
    fmt.Println("Months with five weekends:") // for task item 1
    for year := 1900; year <= 2100; year++ {
        var hasOne bool // for extra credit
        for _, month := range []time.Month{1, 3, 5, 7, 8, 10, 12} {
            t := time.Date(year, month, 1, 0, 0, 0, 0, time.UTC)
            if t.Weekday() == time.Friday {
                // task item 1:  show month
                fmt.Println("  ", t.Format("2006 January"))
                n++
                hasOne = true
                last = t
                if first.IsZero() {
                    first = t
                }
            }
        }
        if !hasOne {
            haveNone = append(haveNone, year)
        }
    }
    fmt.Println(n, "total\n") // task item 2: number of months
    // task item 3
    fmt.Println("First five dates of weekends:")
    for i := 0; i < 5; i++ {
        fmt.Println("  ", first.Format("Monday, January 2, 2006"))
        first = first.Add(7 * 24 * time.Hour)
    }
    fmt.Println("Last five dates of weekends:")
    for i := 0; i < 5; i++ {
        fmt.Println("  ", last.Format("Monday, January 2, 2006"))
        last = last.Add(7 * 24 * time.Hour)
    }
    // extra credit
    fmt.Println("\nYears with no months with five weekends:")
    for _, y := range haveNone {
        fmt.Println("  ", y)
    }
    fmt.Println(len(haveNone), "total")
}
