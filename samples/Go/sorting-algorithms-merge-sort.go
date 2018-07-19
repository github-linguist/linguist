package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}
var s = make([]int, len(a)/2+1) // scratch space for merge step

func main() {
    fmt.Println("before:", a)
    mergeSort(a)
    fmt.Println("after: ", a)
}

func mergeSort(a []int) {
    if len(a) < 2 {
        return
    }
    mid := len(a) / 2
    mergeSort(a[:mid])
    mergeSort(a[mid:])
    if a[mid-1] <= a[mid] {
        return
    }
    // merge step, with the copy-half optimization
    copy(s, a[:mid])
    l, r := 0, mid
    for i := 0; ; i++ {
        if s[l] <= a[r] {
            a[i] = s[l]
            l++
            if l == mid {
                break
            }
        } else {
            a[i] = a[r]
            r++
            if r == len(a) {
                copy(a[i+1:], s[l:mid])
                break
            }
        }
    }
    return
}
