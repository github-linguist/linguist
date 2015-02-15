package main

import "fmt"

func main() {
    keys := []string{"a", "b", "c"}
    vals := []int{1, 2, 3}
    hash := map[string]int{}
    for i, key := range keys {
        hash[key] = vals[i]
    }
    fmt.Println(hash)
}
