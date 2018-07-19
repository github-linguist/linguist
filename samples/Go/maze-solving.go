package main

import (
    "bytes"
    "fmt"
    "math/rand"
    "time"
)

type maze struct {
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
    return &maze{c2, h2, v2}
}

func (m *maze) String() string {
    hWall := []byte("+---")
    hOpen := []byte("+   ")
    vWall := []byte("|   ")
    vOpen := []byte("    ")
    rightCorner := []byte("+\n")
    rightWall := []byte("|\n")
    var b []byte
    for r, hw := range m.h2 {
        for _, h := range hw {
            if h == '-' || r == 0 {
                b = append(b, hWall...)
            } else {
                b = append(b, hOpen...)
                if h != '-' && h != 0 {
                    b[len(b)-2] = h
                }
            }
        }
        b = append(b, rightCorner...)
        for c, vw := range m.v2[r] {
            if vw == '|' || c == 0 {
                b = append(b, vWall...)
            } else {
                b = append(b, vOpen...)
                if vw != '|' && vw != 0 {
                    b[len(b)-4] = vw
                }
            }
            if m.c2[r][c] != 0 {
                b[len(b)-2] = m.c2[r][c]
            }
        }
        b = append(b, rightWall...)
    }
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
    const height = 4
    const width = 7
    m := newMaze(height, width)
    m.gen()
    m.solve(
        rand.Intn(height), rand.Intn(width),
        rand.Intn(height), rand.Intn(width))
    fmt.Print(m)
}

func (m *maze) solve(ra, ca, rz, cz int) {
    var rSolve func(ra, ca, dir int) bool
    rSolve = func(r, c, dir int) bool {
        if r == rz && c == cz {
            m.c2[r][c] = 'F'
            return true
        }
        if dir != dn && m.h2[r][c] == 0 {
            if rSolve(r-1, c, up) {
                m.c2[r][c] = '^'
                m.h2[r][c] = '^'
                return true
            }
        }
        if dir != up && r+1 < len(m.h2) && m.h2[r+1][c] == 0 {
            if rSolve(r+1, c, dn) {
                m.c2[r][c] = 'v'
                m.h2[r+1][c] = 'v'
                return true
            }
        }
        if dir != lf && c+1 < len(m.v2[0]) && m.v2[r][c+1] == 0 {
            if rSolve(r, c+1, rt) {
                m.c2[r][c] = '>'
                m.v2[r][c+1] = '>'
                return true
            }
        }
        if dir != rt && m.v2[r][c] == 0 {
            if rSolve(r, c-1, lf) {
                m.c2[r][c] = '<'
                m.v2[r][c] = '<'
                return true
            }
        }
        return false
    }
    rSolve(ra, ca, -1)
    m.c2[ra][ca] = 'S'
}
