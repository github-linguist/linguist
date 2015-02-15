package main

import (
    "errors"
    "fmt"
    "log"
    "math"
    "time"
)

var inputs = []string{"23:00:17", "23:40:20", "00:12:45", "00:17:19"}

func main() {
    tList := make([]time.Time, len(inputs))
    const clockFmt = "15:04:05"
    var err error
    for i, s := range inputs {
        tList[i], err = time.Parse(clockFmt, s)
        if err != nil {
            log.Fatal(err)
        }
    }
    mean, err := meanTime(tList)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(mean.Format(clockFmt))
}

func meanTime(times []time.Time) (mean time.Time, err error) {
    if len(times) == 0 {
        err = errors.New("meanTime: no times specified")
        return
    }
    var ssum, csum float64
    for _, t := range times {
        h, m, s := t.Clock()
        n := t.Nanosecond()
        fSec := (float64((h*60+m)*60+s) + float64(n)*1e-9)
        sin, cos := math.Sincos(fSec * math.Pi / (12 * 60 * 60))
        ssum += sin
        csum += cos
    }
    if ssum == 0 && csum == 0 {
        err = errors.New("meanTime: mean undefined")
        return
    }
    _, dayFrac := math.Modf(1 + math.Atan2(ssum, csum)/(2*math.Pi))
    return mean.Add(time.Duration(dayFrac * 24 * float64(time.Hour))), nil
}
