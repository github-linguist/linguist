package main

import (
    "fmt"
    "time"
)

const taskDate = "March 7 2009 7:30pm EST"
const taskFormat = "January 2 2006 3:04pm MST"

func main() {
    if etz, err := time.LoadLocation("US/Eastern"); err == nil {
        time.Local = etz
    }
    fmt.Println("Input:             ", taskDate)
    t, err := time.Parse(taskFormat, taskDate)
    if err != nil {
        fmt.Println(err)
        return
    }
    t = t.Add(12 * time.Hour)
    fmt.Println("+12 hrs:           ", t)
    if _, offset := t.Zone(); offset == 0 {
        fmt.Println("No time zone info.")
        return
    }
    atz, err := time.LoadLocation("US/Arizona")
    if err == nil {
        fmt.Println("+12 hrs in Arizona:", t.In(atz))
    }
}
