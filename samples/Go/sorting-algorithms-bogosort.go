package main

import (
    "fmt"
    "math/rand"
    "sort"
    "time"
)

func main() {
    list := []int{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    rand.Seed(time.Now().UnixNano())
    fmt.Println("unsorted:", list)
    temp := make([]int, len(list))
    copy(temp, list)
    for !sort.IntsAreSorted(temp) {
        for i, v := range rand.Perm(len(list)) {
            temp[i] = list[v]
        }
    }
    fmt.Println("sorted!  ", temp)
}
