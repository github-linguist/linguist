package main

import "fmt"

var m map[int]int

func initMap() {
    m = make(map[int]int)
    m[1] = 1
    m[2] = 1
}

func q(n int) (r int) {
    if r = m[n]; r == 0 {
        r = q(n-q(n-1)) + q(n-q(n-2))
        m[n] = r
    }
    return
}

func main() {
    initMap()
    // task
    for n := 1; n <= 10; n++ {
        showQ(n)
    }
    // task
    showQ(1000)
    // extra credit
    count, p := 0, 1
    for n := 2; n <= 1e5; n++ {
        qn := q(n)
        if qn < p {
            count++
        }
        p = qn
    }
    fmt.Println("count:", count)
    // extra credit
    initMap()
    showQ(1e6)
}

func showQ(n int) {
    fmt.Printf("Q(%d) = %d\n", n, q(n))
}
