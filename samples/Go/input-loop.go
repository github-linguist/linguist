package main

import (
    "bufio"
    "os"
)

func main() {
    in := bufio.NewReader(os.Stdin)
    for {
        s, err := in.ReadString('\n')
        if err != nil {
            break
        }
        _ = s
    }
}
