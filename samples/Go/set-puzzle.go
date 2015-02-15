package main

import (
    "fmt"
    "math/rand"
    "time"
)

var (
    number = []string{"1", "2", "3"}
    color  = []string{"red", "green", "purple"}
    shade  = []string{"solid", "open", "striped"}
    shape  = []string{"oval", "squiggle", "diamond"}
)

type card int

func (c card) String() string {
    return fmt.Sprintf("%s %s %s %s",
        number[c/27],
        color[c/9%3],
        shade[c/3%3],
        shape[c%3])
}

func main() {
    rand.Seed(time.Now().Unix())
    basic()
    advanced()
}

func basic() {
    game("Basic", 9, 4)
}

func advanced() {
    game("Advanced", 12, 6)
}

func game(level string, cards, sets int) {
    // create deck
    d := make([]card, 81)
    for i := range d {
        d[i] = card(i)
    }
    var found [][3]card
    for len(found) != sets {
        found = found[:0]
        // deal
        for i := 0; i < cards; i++ {
            j := rand.Intn(81 - i)
            d[i], d[j] = d[j], d[i]
        }
        //  consider all triplets
        for i := 2; i < cards; i++ {
            c1 := d[i]
            for j := 1; j < i; j++ {
                c2 := d[j]
            l3:
                for _, c3 := range d[:j] {
                    for f := card(1); f < 81; f *= 3 {
                        if (c1/f%3+c2/f%3+c3/f%3)%3 != 0 {
                            continue l3 // not a set
                        }
                    }
                    // it's a set
                    found = append(found, [3]card{c1, c2, c3})
                }
            }
        }
    }
    // found the right number
    fmt.Printf("%s game.  %d cards, %d sets.\n", level, cards, sets)
    fmt.Println("Cards:")
    for _, c := range d[:cards] {
        fmt.Println("  ", c)
    }
    fmt.Println("Sets:")
    for _, s := range found {
        fmt.Println("  ", s[0])
        fmt.Println("  ", s[1])
        fmt.Println("  ", s[2])
        fmt.Println()
    }
}
