package main

import (
    "bytes"
    "fmt"
    "math/rand"
    "time"
)

type maze struct {
    c  []byte   // cell contents
    h  []byte   // horizontal walls above cells
    v  []byte   // vertical walls to the left of cells
    c2 [][]byte // cells by row
    h2 [][]byte // horizontal walls by row (ignore first row)
    v2 [][]byte // vertical walls by row (ignore first of each column)
}

func newMaze(rows, cols int) *maze {
    c := make([]byte, rows*cols)              // all cells
    h := bytes.Repeat([]byte{'-'}, rows*cols) // all horizontal walls
    v := bytes.Repeat([]byte{'|'}, rows*cols) // all vertical walls
    c2 := make([][]byte, rows)                // cells by row
    h2 := make([][]byte, rows)                // horizontal walls by row
    v2 := make([][]byte, rows)                // vertical walls by row
    for i := range h2 {
        c2[i] = c[i*cols : (i+1)*cols]
        h2[i] = h[i*cols : (i+1)*cols]
        v2[i] = v[i*cols : (i+1)*cols]
    }
    return &maze{c, h, v, c2, h2, v2}
}

func (m *maze) String() string {
    hWall := []byte("+---")
    hOpen := []byte("+   ")
    vWall := []byte("|   ")
    vOpen := []byte("    ")
    rightCorner := []byte("+\n")
    rightWall := []byte("|\n")
    var b []byte
    // for all rows
    for r, hw := range m.h2 {
        // draw h walls
        for _, h := range hw {
            if h == '-' || r == 0 {
                b = append(b, hWall...)
            } else {
                b = append(b, hOpen...)
            }
        }
        b = append(b, rightCorner...)
        // draw v walls
        for c, vw := range m.v2[r] {
            if vw == '|' || c == 0 {
                b = append(b, vWall...)
            } else {
                b = append(b, vOpen...)
            }
            // draw cell contents
            if m.c2[r][c] != 0 {
                b[len(b)-2] = m.c2[r][c]
            }
        }
        b = append(b, rightWall...)
    }
    // draw bottom edge of maze
    for _ = range m.h2[0] {
        b = append(b, hWall...)
    }
    b = append(b, rightCorner...)
    return string(b)
}

func (m *maze) gen() {
    m.g2(rand.Intn(len(m.c2)), rand.Intn(len(m.c2[0])))
}

const (
    up = iota
    dn
    rt
    lf
)

func (m *maze) g2(r, c int) {
    m.c2[r][c] = ' '
    for _, dir := range rand.Perm(4) {
        switch dir {
        case up:
            if r > 0 && m.c2[r-1][c] == 0 {
                m.h2[r][c] = 0
                m.g2(r-1, c)
            }
        case lf:
            if c > 0 && m.c2[r][c-1] == 0 {
                m.v2[r][c] = 0
                m.g2(r, c-1)
            }
        case dn:
            if r < len(m.c2)-1 && m.c2[r+1][c] == 0 {
                m.h2[r+1][c] = 0
                m.g2(r+1, c)
            }
        case rt:
            if c < len(m.c2[0])-1 && m.c2[r][c+1] == 0 {
                m.v2[r][c+1] = 0
                m.g2(r, c+1)
            }
        }
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    m := newMaze(4, 6)
    m.gen()
    fmt.Print(m)
}
