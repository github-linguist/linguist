package main

import (
    "errors"
    "fmt"
    "unicode"
)

var code = []byte("01230127022455012623017202")

func soundex(s string) (string, error) {
    var sx [4]byte
    var sxi int
    var cx, lastCode byte
    for i, c := range s {
        switch {
        case !unicode.IsLetter(c):
            if c < ' ' || c == 127 {
                return "", errors.New("ASCII control characters disallowed")
            }
            if i == 0 {
                return "", errors.New("initial character must be a letter")
            }
            lastCode = '0'
            continue
        case c >= 'A' && c <= 'Z':
            cx = byte(c - 'A')
        case c >= 'a' && c <= 'z':
            cx = byte(c - 'a')
        default:
            return "", errors.New("non-ASCII letters unsupported")
        }
        // cx is valid letter index at this point
        if i == 0 {
            sx[0] = cx + 'A'
            sxi = 1
            continue
        }
        switch x := code[cx]; x {
        case '7', lastCode:
        case '0':
            lastCode = '0'
        default:
            sx[sxi] = x
            if sxi == 3 {
                return string(sx[:]), nil
            }
            sxi++
            lastCode = x
        }
    }
    if sxi == 0 {
        return "", errors.New("no letters present")
    }
    for ; sxi < 4; sxi++ {
        sx[sxi] = '0'
    }
    return string(sx[:]), nil
}

func main() {
    for _, s := range []string{
        "Robert",   // WP test case = R163
        "Rupert",   // WP test case = R163
        "Rubin",    // WP test case = R150
        "ashcroft", // WP test case = A261
        "ashcraft", // s and c combine across h, t not needed
        "moses",    // s's don't combine across e
        "O'Mally",  // apostrophe allowed, adjacent ll's combine
        "d jay",    // spaces allowed
        "R2-D2",    // digits, hyphen allowed
        "12p2",     // just not in leading position
        "naïve",    // non ASCII disallowed
        "",         // empty string disallowed
        "bump\t",   // ASCII control characters disallowed
    } {
        if x, err := soundex(s); err == nil {
            fmt.Println("soundex", s, "=", x)
        } else {
            fmt.Printf("\"%s\" fail. %s\n", s, err)
        }
    }
}
