package main

import "fmt"

func menu(choices []string, prompt string) string {
    if len(choices) == 0 {
        return ""
    }
    var c int
    for {
        fmt.Println("")
        for i, s := range choices {
            fmt.Printf("%d.  %s\n", i+1, s)
        }
        fmt.Print(prompt)
        _, err := fmt.Scanln(&c)

        if err == nil && c > 0 && c <= len(choices) {
            break
        }
    }
    return choices[c-1]
}

func main() {
    pick := menu(nil, "No prompt")
    fmt.Printf("No choices, result = %q\n", pick)

    choices := []string{
        "fee fie",
        "huff and puff",
        "mirror mirror",
        "tick tock",
    }
    pick = menu(choices, "Enter number: ")
    fmt.Printf("You picked %q\n", pick)
}
