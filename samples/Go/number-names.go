package main

import "fmt"

func main() {
    for _, n := range []int64{12, 1048576, 9e18} {
        fmt.Println(say(n))
    }
}

var small = []string{"", "one", "two", "three", "four", "five", "six",
    "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen",
    "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"}
var tens = []string{"ones", "ten", "twenty", "thirty", "forty",
    "fifty", "sixty", "seventy", "eighty", "ninety"}
var illions = []string{"thousand", "million", "billion",
    "trillion", "quadrillion", "quintillion"}

func say(n int64) string {
    switch {
    case n < 1:
    case n < 20:
        return small[n]
    case n < 100:
        t := tens[n/10]
        s := n % 10
        if s > 0 {
            t += " " + small[s]
        }
        return t
    case n < 1000:
        h := small[n/100] + " hundred"
        s := n % 100
        if s > 0 {
            h += " " + say(s)
        }
        return h
    default:
        sx := say(n % 1000)
        for i := 0; n >= 1000; i++ {
            n /= 1000
            p := n % 1000
            if p > 0 {
                ix := say(p) + " " + illions[i]
                if sx > "" {
                    ix += " " + sx
                }
                sx = ix
            }
        }
        return sx
    }
    return ""
}
