package main

import (
    "fmt"
    "math"
    "math/rand"
    "os"
    "strconv"
    "time"
)

const sSuits = "CDHS"
const sNums = "A23456789TJQK"
const rMax32 = math.MaxInt32

var seed = 1

func rnd() int {
    seed = (seed*214013 + 2531011) & rMax32
    return seed >> 16
}

func deal(s int) []int {
    seed = s
    t := make([]int, 52)
    for i := 0; i < 52; i++ {
        t[i] = 51 - i
    }
    for i := 0; i < 51; i++ {
        j := 51 - rnd()%(52-i)
        t[i], t[j] = t[j], t[i]
    }
    return t
}

func show(cs []int) {
    for i, c := range cs {
        fmt.Printf(" %c%c", sNums[c/4], sSuits[c%4])
        if (i+1)%8 == 0 || i+1 == len(cs) {
            fmt.Println()
        }
    }
}

func main() {
    var game int
    switch len(os.Args) {
    case 1:
        rand.Seed(time.Now().UnixNano())
        game = 1 + rand.Intn(32000)
    case 2:
        var err error
        game, err = strconv.Atoi(os.Args[1])
        if err == nil && game >= 1 && game <= 32000 {
            break
        }
        fallthrough
    default:
        fmt.Println("usage: deal [game]")
        fmt.Println("       where game is a number in the range 1 to 32000")
        return
    }
    fmt.Printf("\nGame #%d\n", game)
    show(deal(game))
}
