package main

import "fmt"

func main() {
	cardinality := func (i int) string {
		if i!=1 {
			return "s"
		}
		return ""
	}
	for i := 99; i > 0; i-- {
		fmt.Printf("%d bottle%s of beer on the wall\n", i, cardinality(i))
		fmt.Printf("%d bottle%s of beer\n", i, cardinality(i))
		fmt.Printf("Take one down, pass it around\n")
		fmt.Printf("%d bottle%s of beer on the wall\n", i-1, cardinality(i-1))
	}
}
