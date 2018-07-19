package main

import "fmt"

func f(s1, s2, sep string) string {
	return s1+sep+sep+s2
}

func main() {
	fmt.Println(f("Rosetta", "Code", ":"))
}
