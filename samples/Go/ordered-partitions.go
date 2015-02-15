package main

import (
	"fmt"
	"os"
	"strconv"
)

func gen_part(n, res []int, pos int) {
	if pos == len(res) {
		x := make([][]int, len(n))
		for i, c := range res {
			x[c] = append(x[c], i+1)
		}

		fmt.Println(x)
		return
	}

	for i := range n {
		if n[i] == 0 {
			continue
		}
		n[i], res[pos] = n[i]-1, i
		gen_part(n, res, pos+1)
		n[i]++
	}
}

func ordered_part(n_parts []int) {
	fmt.Println("Ordered", n_parts)

	sum := 0
	for _, c := range n_parts {
		sum += c
	}

	gen_part(n_parts, make([]int, sum), 0)
}

func main() {
	if len(os.Args) < 2 {
		ordered_part([]int{2, 0, 2})
		return
	}
	n := make([]int, len(os.Args)-1)
	var err error
	for i, a := range os.Args[1:] {
		n[i], err = strconv.Atoi(a)
		if err != nil {
			fmt.Println(err)
			return
		}
		if n[i] < 0 {
			fmt.Println("negative partition size not meaningful")
			return
		}
	}
	ordered_part(n)
}
