package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strings"
)

func main() {
	f, err := os.Open("unixdict.txt")
	if err != nil {
		log.Fatalln(err)
	}
	defer f.Close()

	s := bufio.NewScanner(f)
	rie := regexp.MustCompile("^ie|[^c]ie")
	rei := regexp.MustCompile("^ei|[^c]ei")
	var cie, ie int
	var cei, ei int
	for s.Scan() {
		line := s.Text()
		if strings.Contains(line, "cie") {
			cie++
		}
		if strings.Contains(line, "cei") {
			cei++
		}
		if rie.MatchString(line) {
			ie++
		}
		if rei.MatchString(line) {
			ei++
		}
	}
	err = s.Err()
	if err != nil {
		log.Fatalln(err)
	}

	if check(ie, ei, "I before E when not preceded by C") &&
		check(cei, cie, "E before I when preceded by C") {
		fmt.Println("Both plausable.")
		fmt.Println(`"I before E, except after C" is plausable.`)
	} else {
		fmt.Println("One or both implausable.")
		fmt.Println(`"I before E, except after C" is implausable.`)
	}
}

// check checks if a statement is plausible. Something is plausible if a is more
// than two times b.
func check(a, b int, s string) bool {
	switch {
	case a > b*2:
		fmt.Printf("%q is plausible (%d vs %d).\n", s, a, b)
		return true
	case a >= b:
		fmt.Printf("%q is implausible (%d vs %d).\n", s, a, b)
	default:
		fmt.Printf("%q is implausible and contra-indicated (%d vs %d).\n",
			s, a, b)
	}
	return false
}
