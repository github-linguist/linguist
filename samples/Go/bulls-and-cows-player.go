package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    fmt.Println(`Cows and bulls/player
You think of four digit number of unique digits in the range 1 to 9.
I guess.  You score my guess:
    A correct digit but not in the correct place is a cow.
    A correct digit in the correct place is a bull.
You give my score as two numbers separated with a space.`)

    // generate possible patterns, store in map
    m := make(map[string]int)
    var g func([]byte, int)
    g = func(digits []byte, fixed int) {
        if fixed == 4 {
            m[string(digits[:4])] = 0
            return
        }
        for i := fixed; i < len(digits); i++ {
            digits[fixed], digits[i] = digits[i], digits[fixed]
            g(digits, fixed+1)
            digits[fixed], digits[i] = digits[i], digits[fixed]
        }
    }
    g([]byte("123456789"), 0)

    // guess/score/eliminate loop
    for in := bufio.NewReader(os.Stdin);; {
        // pick a value, ie, guess
        var guess string
        for guess = range m {
            delete(m, guess)
            break
        }

        // get and parse score
        var c, b uint
        for ;; fmt.Println("Score guess as two numbers: cows bulls") {
            fmt.Printf("My guess: %s.  Score? (c b) ", guess)
            score, err := in.ReadString('\n')
            if err != nil {
                fmt.Println("\nSo, bye.")
                return
            }
            s2 := strings.Fields(score)
            if len(s2) == 2 {
                c2, err := strconv.ParseUint(s2[0], 10, 0)
                if err == nil && c2 <= 4 {
                    b2, err := strconv.ParseUint(s2[1], 10, 0)
                    if err == nil && c2+b2 <= 4 {
                        c = uint(c2)
                        b = uint(b2)
                        break
                    }
                }
            }
        }

        // check for win
        if b == 4 {
            fmt.Println("I did it. :)")
            return
        }

        // eliminate patterns with non-matching scores
        for pat := range m {
            var cows, bulls uint
            for ig, cg := range guess {
                switch strings.IndexRune(pat, cg) {
                case -1:
                default: // I just think cows should go first
                    cows++
                case ig:
                    bulls++
                }
            }
            if cows != c || bulls != b {
                delete(m, pat)
            }
        }

        // check for inconsistency
        if len(m) == 0 {
            fmt.Println("Oops, check scoring.")
            return
        }
    }
}
