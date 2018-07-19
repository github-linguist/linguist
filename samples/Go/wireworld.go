package main

import (
    "bytes"
    "fmt"
    "io/ioutil"
    "strings"
)

var rows, cols int // extent of input configuration
var rx, cx int     // grid extent (includes border)
var mn []int       // offsets of moore neighborhood

func main() {
    // read input configuration from file
    src, err := ioutil.ReadFile("ww.config")
    if err != nil {
        fmt.Println(err)
        return
    }
    srcRows := bytes.Split(src, []byte{'\n'})

    // compute package variables
    rows = len(srcRows)
    for _, r := range srcRows {
        if len(r) > cols {
            cols = len(r)
        }
    }
    rx, cx = rows+2, cols+2
    mn = []int{-cx-1, -cx, -cx+1, -1, 1, cx-1, cx, cx+1}

    // allocate two grids and copy input into first grid
    odd := make([]byte, rx*cx)
    even := make([]byte, rx*cx)
    for ri, r := range srcRows {
        copy(odd[(ri+1)*cx+1:], r)
    }

    // run
    for {
        print(odd)
        step(even, odd)
        fmt.Scanln()

        print(even)
        step(odd, even)
        fmt.Scanln()
    }
}

func print(grid []byte) {
    fmt.Println(strings.Repeat("__", cols))
    fmt.Println()
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            if grid[r*cx+c] == 0 {
                fmt.Print("  ")
            } else {
                fmt.Printf(" %c", grid[r*cx+c])
            }
        }
        fmt.Println()
    }
}

func step(dst, src []byte) {
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            x := r*cx + c
            dst[x] = src[x]
            switch dst[x] {
            case 'H':
                dst[x] = 't'
            case 't':
                dst[x] = '.'
            case '.':
                var nn int
                for _, n := range mn {
                    if src[x+n] == 'H' {
                        nn++
                    }
                }
                if nn == 1 || nn == 2 {
                    dst[x] = 'H'
                }
            }
        }
    }
}
