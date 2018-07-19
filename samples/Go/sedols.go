package main

import (
    "fmt"
    "strings"
    "strconv"
)

const input = `710889
B0YBKJ
406566
B0YBLH
228276
B0YBKL
557910
B0YBKR
585284
B0YBKT
B00030

B
B0003
B000300
A00030
E00030
I00030
O00030
U00030
β00030
β0003`

var weight = [...]int{1,3,1,7,3,9}

func csd(code string) string {
    switch len(code) {
    case 6:
    case 0:
        return "No data"
    default:
        return "Invalid length"
    }
    sum := 0
    for i, c := range code {
        n, err := strconv.ParseInt(string(c), 36, 0)
        if err != nil || c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' {
            return "Invalid character"
        }
        sum += int(n)*weight[i]
    }
    return strconv.Itoa(9-(sum-1)%10)
}

func main() {
    for _, s := range strings.Split(input, "\n") {
        d := csd(s)
        if len(d) > 1 {
            fmt.Printf(":%s: %s\n", s, d)
        } else {
            fmt.Println(s + d)
        }
    }
}
