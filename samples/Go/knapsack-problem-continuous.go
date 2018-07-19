package main

import (
    "fmt"
    "sort"
)

type item struct {
    item   string
    weight float64
    price  float64
}

type items []item

var all = items{
    {"beef", 3.8, 36},
    {"pork", 5.4, 43},
    {"ham", 3.6, 90},
    {"greaves", 2.4, 45},
    {"flitch", 4.0, 30},
    {"brawn", 2.5, 56},
    {"welt", 3.7, 67},
    {"salami", 3.0, 95},
    {"sausage", 5.9, 98},
}

// satisfy sort interface
func (z items) Len() int      { return len(z) }
func (z items) Swap(i, j int) { z[i], z[j] = z[j], z[i] }
func (z items) Less(i, j int) bool {
    return z[i].price/z[i].weight > z[j].price/z[j].weight
}

func main() {
    left := 15.
    sort.Sort(all)
    for _, i := range all {
        if i.weight <= left {
            fmt.Println("take all the", i.item)
            if i.weight == left {
                return
            }
            left -= i.weight
        } else {
            fmt.Printf("take %.1fkg %s\n", left, i.item)
            return
        }
    }
}
