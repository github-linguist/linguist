package main

import "fmt"

func main() {
    demoPerm(3)
}

func demoPerm(n int) {
    // create a set to permute.  for demo, use the integers 1..n.
    s := make([]int, n)
    for i := range s {
        s[i] = i + 1
    }
    // permute them, calling a function for each permutation.
    // for demo, function just prints the permutation.
    permute(s, func(p []int) { fmt.Println(p) })
}

// permute function.  takes a set to permute and a function
// to call for each generated permutation.
func permute(s []int, emit func([]int)) {
    if len(s) == 0 {
        emit(s)
        return
    }
    // Steinhaus, implemented with a recursive closure.
    // arg is number of positions left to permute.
    // pass in len(s) to start generation.
    // on each call, weave element at pp through the elements 0..np-2,
    // then restore array to the way it was.
    var rc func(int)
    rc = func(np int) {
        if np == 1 {
            emit(s)
            return
        }
        np1 := np - 1
        pp := len(s) - np1
        // weave
        rc(np1)
        for i := pp; i > 0; i-- {
            s[i], s[i-1] = s[i-1], s[i]
            rc(np1)
        }
        // restore
        w := s[0]
        copy(s, s[1:pp+1])
        s[pp] = w
    }
    rc(len(s))
}
