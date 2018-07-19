package main

import (
    "bufio"
    "fmt"
    "io"
    "os"
    "bytes"
)

var fn = "mlijobs.txt"

func main() {
    f, err := os.Open(fn)
    if err != nil {
        fmt.Println(err)
        return
    }
    defer f.Close()
    var ml, out int
    var mlTimes []string
    in := []byte("IN")
    for lr := bufio.NewReader(f); ; {
        line, pref, err := lr.ReadLine()
        if err == io.EOF {
            break
        }
        if err != nil {
            fmt.Println(err)
            return
        }
        if pref {
            fmt.Println("Unexpected long line.")
            return
        }
        f := bytes.Fields(line)
        if len(f) != 7 {
            fmt.Println("unexpected format,", len(f), "fields.")
            return
        }
        if bytes.Equal(f[1], in) {
            out--
            continue
        }
        out++
        if out == ml {
            mlTimes = append(mlTimes, string(f[3]))
            continue
        }
        if out > ml {
            ml = out
            mlTimes = append(mlTimes[:0], string(f[3]))
        }
    }
    fmt.Println("max licenses:", ml)
    fmt.Println("at:")
    for _, t := range mlTimes {
        fmt.Println(" ", t)
    }
}
