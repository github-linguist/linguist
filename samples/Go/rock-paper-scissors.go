package main

import (
    "fmt"
    "math/rand"
    "strings"
    "time"
)

const rps = "rps"

var msg = []string{
    "Rock breaks scissors",
    "Paper covers rock",
    "Scissors cut paper",
}

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println("Rock Paper Scissors")
    fmt.Println("Enter r, p, or s as your play.  Anything else ends the game.")
    fmt.Println("Running score shown as <your wins>:<my wins>")
    var pi string // player input
    var aScore, pScore int
    sl := 3               // for output alignment
    pcf := make([]int, 3) // pcf = player choice frequency
    var plays int
    aChoice := rand.Intn(3) // ai choice for first play is completely random
    for {
        // get player choice
        fmt.Print("Play: ")
        _, err := fmt.Scanln(&pi)  // lazy
        if err != nil || len(pi) != 1 {
            break
        }
        pChoice := strings.Index(rps, pi)
        if pChoice < 0 {
            break
        }
        pcf[pChoice]++
        plays++

        // show result of play
        fmt.Printf("My play:%s%c.  ", strings.Repeat(" ", sl-2), rps[aChoice])
        switch (aChoice - pChoice + 3) % 3 {
        case 0:
            fmt.Println("Tie.")
        case 1:
            fmt.Printf("%s.  My point.\n", msg[aChoice])
            aScore++
        case 2:
            fmt.Printf("%s.  Your point.\n", msg[pChoice])
            pScore++
        }

        // show score
        sl, _ = fmt.Printf("%d:%d  ", pScore, aScore)

        // compute ai choice for next play
        switch rn := rand.Intn(plays); {
        case rn < pcf[0]:
            aChoice = 1
        case rn < pcf[0]+pcf[1]:
            aChoice = 2
        default:
            aChoice = 0
        }
    }
}
