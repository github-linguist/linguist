package main

import (
    "os"
    "os/exec"
)

func main() {
    c := exec.Command("clear")
    c.Stdout = os.Stdout
    c.Run()
}
