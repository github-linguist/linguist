package main

import (
    "fmt"
    "bufio"
    "os"
    "strconv"
    "strings"
)

func main() {
    in := bufio.NewReader(os.Stdin)
    getInt := func() int {
        fmt.Print("Integer: ")
        s, err := in.ReadString('\n')
        if err != nil {
            fmt.Println("\nComputer says input error")
            os.Exit(0)
        }
        i, err := strconv.Atoi(strings.TrimSpace(s))
        if err != nil {
            fmt.Println("Not an integer")
            os.Exit(0)
        }
        return i
    }

    n1 := getInt()
    n2 := getInt()

    switch {
    case n1 < n2:  fmt.Println(n1, "less than", n2)
    case n1 == n2: fmt.Println(n1, "equal to", n2)
    case n1 > n2:  fmt.Println(n1, "greater than", n2)
    }
}
