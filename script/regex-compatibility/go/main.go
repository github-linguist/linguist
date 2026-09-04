package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"regexp"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "usage: regex-compatibility-go PATTERNS_FILE")
		os.Exit(2)
	}

	data, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	if len(data) == 0 {
		return
	}

	patterns := bytes.Split(data, []byte{0})
	if len(patterns) > 0 && len(patterns[len(patterns)-1]) == 0 {
		patterns = patterns[:len(patterns)-1]
	}

	output := bufio.NewWriter(os.Stdout)
	for index, pattern := range patterns {
		if _, err := regexp.Compile(string(pattern)); err != nil {
			fmt.Fprintf(output, "%d%c%s%c", index, 0, err, 0)
		}
	}
	if err := output.Flush(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
}
