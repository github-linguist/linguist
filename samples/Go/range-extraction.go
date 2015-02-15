package main

import (
    "errors"
    "fmt"
    "strconv"
    "strings"
)

func main() {
    rf, err := rangeFormat([]int{
        0, 1, 2, 4, 6, 7, 8, 11, 12, 14,
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
        37, 38, 39,
    })
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("range format:", rf)
}

func rangeFormat(a []int) (string, error) {
    if len(a) == 0 {
        return "", nil
    }
    var parts []string
    for n1 := 0; ; {
        n2 := n1 + 1
        for n2 < len(a) && a[n2] == a[n2-1]+1 {
            n2++
        }
        s := strconv.Itoa(a[n1])
        if n2 == n1+2 {
            s += "," + strconv.Itoa(a[n2-1])
        } else if n2 > n1+2 {
            s += "-" + strconv.Itoa(a[n2-1])
        }
        parts = append(parts, s)
        if n2 == len(a) {
            break
        }
        if a[n2] == a[n2-1] {
            return "", errors.New(fmt.Sprintf(
                "sequence repeats value %d", a[n2]))
        }
        if a[n2] < a[n2-1] {
            return "", errors.New(fmt.Sprintf(
                "sequence not ordered: %d < %d", a[n2], a[n2-1]))
        }
        n1 = n2
    }
    return strings.Join(parts, ","), nil
}
