package main

import (
	"fmt"
	"os"
)

func main() {
	host, _ := os.Hostname()
	fmt.Printf("hostname: %s\n", host)
}
