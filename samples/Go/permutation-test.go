package main

import "fmt"

var tr = []int{85, 88, 75, 66, 25, 29, 83, 39, 97}
var ct = []int{68, 41, 10, 49, 16, 65, 32, 92, 28, 98}

func main() {
    // collect all results in a single list
    all := make([]int, len(tr)+len(ct))
    copy(all, tr)
    copy(all[len(tr):], ct)

    // compute sum of all data, useful as intermediate result
    var sumAll int
    for _, r := range all {
        sumAll += r
    }

    // closure for computing scaled difference.
    // compute results scaled by len(tr)*len(ct).
    // this allows all math to be done in integers.
    sd := func(trc []int) int {
        var sumTr int
        for _, x := range trc {
            sumTr += all[x]
        }
        return sumTr*len(ct) - (sumAll-sumTr)*len(tr)
    }

    // compute observed difference, as an intermediate result
    a := make([]int, len(tr))
    for i, _ := range a {
        a[i] = i
    }
    sdObs := sd(a)

    // iterate over all combinations.  for each, compute (scaled)
    // difference and tally whether leq or gt observed difference.
    var nLe, nGt int
    comb(len(all), len(tr), func(c []int) {
        if sd(c) > sdObs {
            nGt++
        } else {
            nLe++
        }
    })

    // print results as percentage
    pc := 100 / float64(nLe+nGt)
    fmt.Printf("differences <= observed: %f%%\n", float64(nLe)*pc)
    fmt.Printf("differences  > observed: %f%%\n", float64(nGt)*pc)
}

// combination generator, copied from combination task
func comb(n, m int, emit func([]int)) {
    s := make([]int, m)
    last := m - 1
    var rc func(int, int)
    rc = func(i, next int) {
        for j := next; j < n; j++ {
            s[i] = j
            if i == last {
                emit(s)
            } else {
                rc(i+1, j+1)
            }
        }
        return
    }
    rc(0, 0)
}
