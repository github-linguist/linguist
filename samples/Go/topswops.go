// Adapted from http://www-cs-faculty.stanford.edu/~uno/programs/topswops.w
// at Donald Knuth's web site.  Algorithm credited there to Pepperdine
// and referenced to Mathematical Gazette 73 (1989), 131-133.
package main

import "fmt"

const ( // array sizes
    maxn = 10 // max number of cards
    maxl = 50 // upper bound for number of steps
)

func main() {
    for i := 1; i <= maxn; i++ {
        fmt.Printf("%d: %d\n", i, steps(i))
    }
}

func steps(n int) int {
    var a, b [maxl][maxn + 1]int
    var x [maxl]int
    a[0][0] = 1
    var m int
    for l := 0; ; {
        x[l]++
        k := int(x[l])
        if k >= n {
            if l <= 0 {
                break
            }
            l--
            continue
        }
        if a[l][k] == 0 {
            if b[l][k+1] != 0 {
                continue
            }
        } else if a[l][k] != k+1 {
            continue
        }
        a[l+1] = a[l]
        for j := 1; j <= k; j++ {
            a[l+1][j] = a[l][k-j]
        }
        b[l+1] = b[l]
        a[l+1][0] = k + 1
        b[l+1][k+1] = 1
        if l > m-1 {
            m = l + 1
        }
        l++
        x[l] = 0
    }
    return m
}
