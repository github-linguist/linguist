package main

import (
    "fmt"
    "strings"
)

var lean = font{
    height:  5,
    slant:   1,
    spacing: 2,
    m: map[rune][]string{
        'G': []string{
            `  _/_/_/`,
            `_/      `,
            `_/  _/_/`,
            `_/    _/`,
            `  _/_/_/`,
        },
        'o': []string{
            `        `,
            `  _/_/  `,
            `_/    _/`,
            `_/    _/`,
            `  _/_/  `,
        },
    }}

var smallKeyboard = font{
    height:  4,
    slant:   0,
    spacing: -1,
    m: map[rune][]string{
        'G': []string{
            ` ____ `,
            `||G ||`,
            `||__||`,
            `|/__\|`,
        },
        'o': []string{
            ` ____ `,
            `||o ||`,
            `||__||`,
            `|/__\|`,
        },
    }}

type font struct {
    height  int
    slant   int
    spacing int
    m       map[rune][]string
}

func render(s string, f font) string {
    rows := make([]string, f.height)
    if f.slant != 0 {
        start := 0
        if f.slant > 0 {
            start = f.height
        }
        for i := range rows {
            rows[i] = strings.Repeat(" ", (start-i)*f.slant)
        }
    }
    if f.spacing >= 0 {
        spacing := strings.Repeat(" ", f.spacing)
        for j, c := range s {
            for i, r := range f.m[c] {
                if j > 0 {
                    r = spacing + r
                }
                rows[i] += r
            }
        }
    } else {
        overlap := -f.spacing
        for j, c := range s {
            for i, r := range f.m[c] {
                if j > 0 {
                    r = r[overlap:]
                }
                rows[i] += r
            }
        }
    }
    return strings.Join(rows, "\n")
}

func main() {
    fmt.Println(render("Go", lean))
    fmt.Println(render("Go", smallKeyboard))
}
