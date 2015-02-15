package main

import (
    "fmt"
    "time"
)

type holiday struct {
    time.Time
}

func easter(y int) holiday {
    c := y / 100
    n := mod(y, 19)
    i := mod(c-c/4-(c-(c-17)/25)/3+19*n+15, 30)
    i -= (i / 28) * (1 - (i/28)*(29/(i+1))*((21-n)/11))
    l := i - mod(y+y/4+i+2-c+c/4, 7)
    m := 3 + (l+40)/44
    d := l + 28 - 31*(m/4)
    return holiday{time.Date(y, time.Month(m), d, 0, 0, 0, 0, time.UTC)}
}

func mod(a, n int) int {
    r := a % n
    if r < 0 {
        return r + n
    }
    return r
}

func (h holiday) addDays(d int) holiday {
    return holiday{h.Add(time.Duration(d) * 24 * time.Hour)}
}

type easterRelated struct {
    easter, ascension, pentecost, trinity, corpusChristi holiday
}

func newEasterRelated(y int) (er easterRelated) {
    er.easter = easter(y)
    er.ascension = er.easter.addDays(39)
    er.pentecost = er.ascension.addDays(10)
    er.trinity = er.pentecost.addDays(7)
    er.corpusChristi = er.trinity.addDays(4)
    return
}

func (er easterRelated) print() {
    const wdmdm = ": Mon _2 Jan"
    fmt.Printf("%4d %s, %s, %s, %s, %s\n",
        er.easter.Year(),
        er.easter.Format("Easter"+wdmdm),
        er.ascension.Format("Ascension"+wdmdm),
        er.pentecost.Format("Pentecost"+wdmdm),
        er.trinity.Format("Trinity"+wdmdm),
        er.corpusChristi.Format("Corpus"+wdmdm))
}

func main() {
    fmt.Println("Christian holidays, related to Easter, " +
        "for each centennial from 400 to 2100 CE:")
    for y := 400; y <= 2100; y += 100 {
        newEasterRelated(y).print()
    }
    fmt.Println("\nChristian holidays, related to Easter, " +
        "for years from 2010 to 2020 CE:")
    for y := 2010; y <= 2020; y++ {
        newEasterRelated(y).print()
    }
}
