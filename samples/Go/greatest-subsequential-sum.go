package main

import "fmt"

func gss(s []int) ([]int, int) {
    var best, start, end, sum, sumStart int
    for i, x := range s {
        sum += x
        switch {
        case sum > best:
            best = sum
            start = sumStart
            end = i + 1
        case sum < 0:
            sum = 0
            sumStart = i + 1
        }
    }
    return s[start:end], best
}

var testCases = [][]int{
    {-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1},
    {-1, 1, 2, -5, -6},
    {},
    {-1, -2, -1},
}

func main() {
    for _, c := range testCases {
        fmt.Println("Input:  ", c)
        subSeq, sum := gss(c)
        fmt.Println("Sub seq:", subSeq)
        fmt.Println("Sum:    ", sum, "\n")
    }
}
