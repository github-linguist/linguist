package main

import (
    "bufio"
    "errors"
    "fmt"
    "io"
    "os"
)

func main() {
    if line, err := rsl("input.txt", 7); err == nil {
        fmt.Println("7th line:")
        fmt.Println(line)
    } else {
        fmt.Println(err)
    }
}

func rsl(fn string, n int) (r string, err error) {
    if n < 1 {
        return "", errors.New(fmt.Sprintf("Invalid request:  line %d", n))
    }
    f, err := os.Open(fn)
    if err != nil {
        return
    }
    defer f.Close()
    bf := bufio.NewReader(f)
    var l int
    for {
        line, isPrefix, err := bf.ReadLine()
        if err == io.EOF {
            switch l {
            case 0:
                return "", errors.New("Empty file.")
            case 1:
                return "", errors.New("Only 1 line.")
            default:
                return "", errors.New(fmt.Sprintf("Only %d lines", l))
            }
        }
        if err != nil {
            return "", err
        }
        l++
        if l == n {
            r = string(line)
            for isPrefix {
                line, isPrefix, err = bf.ReadLine()
                if err != nil {
                    return "", err
                }
                r += string(line)
            }
            if r == "" {
                return "", errors.New(fmt.Sprintf("Line %d empty.", n))
            }
            break
        }
        for isPrefix {
            line, isPrefix, err = bf.ReadLine()
            if err != nil {
                return "", err
            }
        }
    }
    return // success
}
