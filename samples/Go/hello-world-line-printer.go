package main

import (
	"fmt"
	"os"
)

func main() {
	lp0, err := os.Create("/dev/lp0")

	if err != nil {
		panic(err)
	}

	defer lp0.Close()

	fmt.Fprintln(lp0, "Hello World!")
}
