package main

import (
    "fmt"
    "strings"
)

const text = `Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.`

type formatter struct {
    text  [][]string
    width []int
}

func newFormatter(text string) *formatter {
    var f formatter
    for _, line := range strings.Split(text, "\n") {
        words := strings.Split(line, "$")
        for words[len(words)-1] == "" {
            words = words[:len(words)-1]
        }
        f.text = append(f.text, words)
        for i, word := range words {
            if i == len(f.width) {
                f.width = append(f.width, len(word))
            } else if len(word) > f.width[i] {
                f.width[i] = len(word)
            }
        }
    }
    return &f
}

const (
    left = iota
    middle
    right
)

func (f formatter) print(j int) {
    for _, line := range f.text {
        for i, word := range line {
            fmt.Printf("%-*s ", f.width[i], fmt.Sprintf("%*s",
                len(word)+(f.width[i]-len(word))*j/2, word))
        }
        fmt.Println("")
    }
    fmt.Println("")
}

func main() {
    f := newFormatter(text)
    f.print(left)
    f.print(middle)
    f.print(right)
}
