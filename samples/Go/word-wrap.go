package main

import (
    "fmt"
    "strings"
)

func wrap(text string, lineWidth int) (wrapped string) {
    words := strings.Fields(text)
    if len(words) == 0 {
        return
    }
    wrapped = words[0]
    spaceLeft := lineWidth - len(wrapped)
    for _, word := range words[1:] {
        if len(word)+1 > spaceLeft {
            wrapped += "\n" + word
            spaceLeft = lineWidth - len(word)
        } else {
            wrapped += " " + word
            spaceLeft -= 1 + len(word)
        }
    }
    return
}

var frog = `
In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything.`

func main() {
    fmt.Println("wrapped at 80:")
    fmt.Println(wrap(frog, 80))
    fmt.Println("wrapped at 72:")
    fmt.Println(wrap(frog, 72))
}
