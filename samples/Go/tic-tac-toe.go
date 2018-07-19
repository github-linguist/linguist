package main

import (
    "bufio"
    "fmt"
    "math/rand"
    "os"
    "strings"
)

var b []byte

func printBoard() {
    fmt.Printf("%s\n%s\n%s\n", b[0:3], b[3:6], b[6:9])
}

var pScore, cScore int
var pMark, cMark byte = 'X', 'O'
var in = bufio.NewReader(os.Stdin)

func main() {
    b = make([]byte, 9)
    fmt.Println("Play by entering a digit.")
    for {
        // start of game
        for i := range b {
            b[i] = '1' + byte(i)
        }
        computerStart := cMark == 'X'
        if computerStart {
            fmt.Println("I go first, playing X's")
        } else {
            fmt.Println("You go first, playing X's")
        }
    TakeTurns:
        for {
            if !computerStart {
                if !playerTurn() {
                    return
                }
                if gameOver() {
                    break TakeTurns
                }

            }
            computerStart = false
            computerTurn()
            if gameOver() {
                break TakeTurns
            }
        }
        fmt.Println("Score: you", pScore, "me", cScore)
        fmt.Println("\nLet's play again.")
    }
}

func playerTurn() bool {
    var pm string
    var err error
    for i := 0; i < 3; i++ { // retry loop
        printBoard()
        fmt.Printf("%c's move? ", pMark)
        if pm, err = in.ReadString('\n'); err != nil {
            fmt.Println(err)
            return false
        }
        pm = strings.TrimSpace(pm)
        if pm >= "1" && pm <= "9" && b[pm[0]-'1'] == pm[0] {
            x := pm[0] - '1'
            b[x] = pMark
            return true
        }
    }
    fmt.Println("You're not playing right.")
    return false
}

var choices = make([]int, 9)

func computerTurn() {
    printBoard()
    var x int
    defer func() {
        fmt.Println("My move:", x+1)
        b[x] = cMark
    }()
    // look for two in a row
    block := -1
    for _, l := range lines {
        var mine, yours int
        x = -1
        for _, sq := range l {
            switch b[sq] {
            case cMark:
                mine++
            case pMark:
                yours++
            default:
                x = sq
            }
        }
        if mine == 2 && x >= 0 {
            return // strategy 1: make winning move
        }
        if yours == 2 && x >= 0 {
            block = x
        }
    }
    if block >= 0 {
        x = block // strategy 2: make blocking move
        return
    }
    // default strategy: random move
    choices = choices[:0]
    for i, sq := range b {
        if sq == '1'+byte(i) {
            choices = append(choices, i)
        }
    }
    x = choices[rand.Intn(len(choices))]
}

func gameOver() bool {
    // check for win
    for _, l := range lines {
        if b[l[0]] == b[l[1]] && b[l[1]] == b[l[2]] {
            printBoard()
            if b[l[0]] == cMark {
                fmt.Println("I win!")
                cScore++
                pMark, cMark = 'X', 'O'
            } else {
                fmt.Println("You win!")
                pScore++
                pMark, cMark = 'O', 'X'
            }
            return true
        }
    }
    // check for empty squares
    for i, sq := range b {
        if sq == '1'+byte(i) {
            return false
        }
    }
    fmt.Println("Cat game.")
    pMark, cMark = cMark, pMark
    return true
}

var lines = [][]int{
    {0, 1, 2}, // rows
    {3, 4, 5},
    {6, 7, 8},
    {0, 3, 6}, // columns
    {1, 4, 7},
    {2, 5, 8},
    {0, 4, 8}, // diagonals
    {2, 4, 6},
}
