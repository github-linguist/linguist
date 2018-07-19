package main

import "fmt"

func main() {
    list := pancake{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    fmt.Println("unsorted:", list)

    list.sort()
    fmt.Println("sorted!  ", list)
}

type pancake []int

func (a pancake) sort() {
    for uns := len(a) - 1; uns > 0; uns-- {
        // find largest in unsorted range
        lx, lg := 0, a[0]
        for i := 1; i <= uns; i++ {
            if a[i] > lg {
                lx, lg = i, a[i]
            }
        }
        // move to final position in two flips
        a.flip(lx)
        a.flip(uns)
    }
}

func (a pancake) flip(r int) {
    for l := 0; l < r; l, r = l+1, r-1 {
        a[l], a[r] = a[r], a[l]
    }
}
