package main

import (
    "fmt"
    "log"
    "strconv"

    "digit"
)

type testCase struct {
    n           string
    base        int
    persistence int
    root        int
}

var testCases = []testCase{
    {"627615", 10, 2, 9},
    {"39390", 10, 2, 6},
    {"588225", 10, 2, 3},
    {"393900588225", 10, 2, 9},
}

func root(n string, base int) (persistence, root int, err error) {
    i, err := digit.Sum(n, base)
    if err != nil {
        return 0, 0, err
    }
    if len(n) == 1 {
        return 0, int(i), nil
    }
    for {
        persistence++
        n := strconv.FormatInt(i, base)
        if len(n) == 1 {
            root = int(i)
            break
        }
        i, _ = digit.Sum(n, base)
    }
    return
}

func main() {
    for _, tc := range testCases {
        p, r, err := root(tc.n, tc.base)
        if err != nil {
            log.Fatal(err)
        }
        if p != tc.persistence || r != tc.root {
            log.Fatal("test case", tc)
        }
    }
    fmt.Println("all tests passed")
}
