package main

import "fmt"

// its' not too much more work to check all the permutations concurrently
var solution = make(chan int)
var nearMiss = make(chan int)
var done = make(chan bool)

func main() {
    // iterate and use the bits as the permutation
    for i := 0; i < 4096; i++ {
        go checkPerm(i)
    }
    // collect the misses and list them after the complete solution(s)
    var ms []int
    for i := 0; i < 4096; {
        select {
        case <-done:
            i++
        case s := <-solution:
            print12("solution", s)
        case m := <-nearMiss:
            ms = append(ms, m)
        }
    }
    for _, m := range ms {
        print12("near miss", m)
    }
}

func print12(label string, bits int) {
    fmt.Print(label, ":")
    for i := 1; i <= 12; i++ {
        if bits&1 == 1 {
            fmt.Print(" ", i)
        }
        bits >>= 1
    }
    fmt.Println()
}

func checkPerm(tz int) {
    // closure returns true if tz bit corresponding to
    // 1-based statement number is 1.
    ts := func(n uint) bool {
        return tz>>(n-1)&1 == 1
    }
    // variadic closure returns number of statements listed as arguments
    // which have corresponding tz bit == 1.
    ntrue := func(xs ...uint) int {
        nt := 0
        for _, x := range xs {
            if ts(x) {
                nt++
            }
        }
        return nt
    }
    // a flag used on repeated calls to test.
    // set to true when first contradiction is found.
    // if another is found, this function (checkPerm) can "short circuit"
    // and return immediately without checking additional statements.
    var con bool
    // closure called to test each statement
    test := func(statement uint, b bool) {
        switch {
        case ts(statement) == b:
        case con:
            panic("bail")
        default:
            con = true
        }
    }
    // short circuit mechanism
    defer func() {
        if x := recover(); x != nil {
            if msg, ok := x.(string); !ok && msg != "bail" {
                panic(x)
            }
        }
        done <- true
    }()

    // 1. This is a numbered list of twelve statements.
    test(1, true)

    // 2. Exactly 3 of the last 6 statements are true.
    test(2, ntrue(7, 8, 9, 10, 11, 12) == 3)

    // 3. Exactly 2 of the even-numbered statements are true.
    test(3, ntrue(2, 4, 6, 8, 10, 12) == 2)

    // 4. If statement 5 is true, then statements 6 and 7 are both true.
    test(4, !ts(5) || ts(6) && ts(7))

    // 5. The 3 preceding statements are all false.
    test(5, !ts(4) && !ts(3) && !ts(2))

    // 6. Exactly 4 of the odd-numbered statements are true.
    test(6, ntrue(1, 3, 5, 7, 9, 11) == 4)

    // 7. Either statement 2 or 3 is true, but not both.
    test(7, ts(2) != ts(3))

    // 8. If statement 7 is true, then 5 and 6 are both true.
    test(8, !ts(7) || ts(5) && ts(6))

    // 9. Exactly 3 of the first 6 statements are true.
    test(9, ntrue(1, 2, 3, 4, 5, 6) == 3)

    // 10. The next two statements are both true.
    test(10, ts(11) && ts(12))

    // 11. Exactly 1 of statements 7, 8 and 9 are true.
    test(11, ntrue(7, 8, 9) == 1)

    // 12. Exactly 4 of the preceding statements are true.
    test(12, ntrue(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) == 4)

    // no short circuit? send permutation as either near miss or solution
    if con {
        nearMiss <- tz
    } else {
        solution <- tz
    }
}
