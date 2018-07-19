package main

import (
    "bytes"
    "encoding/xml"
    "fmt"
    "io"
    "net/http"
    "regexp"
    "time"
)

func main() {
    resp, err := http.Get("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
    if err != nil {
        fmt.Println(err) // connection or request fail
        return
    }
    defer resp.Body.Close()
    var us string
    var ux int
    utc := []byte("UTC")
    for p := xml.NewDecoder(resp.Body); ; {
        t, err := p.RawToken()
        switch err {
        case nil:
        case io.EOF:
            fmt.Println("UTC not found")
            return
        default:
            fmt.Println(err) // read or parse fail
            return
        }
        if ub, ok := t.(xml.CharData); ok {
            if ux = bytes.Index(ub, utc); ux != -1 {
                // success: found a line with the string "UTC"
                us = string([]byte(ub))
                break
            }
        }
    }
    // first thing to try: parsing the expected date format
    if t, err := time.Parse("Jan. 2, 15:04:05 UTC", us[:ux+3]); err == nil {
        fmt.Println("parsed UTC:", t.Format("January 2, 15:04:05"))
        return
    }
    // fallback: search for anything looking like a time and print that
    tx := regexp.MustCompile("[0-2]?[0-9]:[0-5][0-9]:[0-6][0-9]")
    if justTime := tx.FindString(us); justTime > "" {
        fmt.Println("found UTC:", justTime)
        return
    }
    // last resort: just print the whole element containing "UTC" and hope
    // there is a human readable time in there somewhere.
    fmt.Println(us)
}
