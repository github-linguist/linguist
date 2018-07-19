package main

import (
    "fmt"
    "math/rand"
    "time"
)

func sOfNCreator(n int) func(byte) []byte {
    s := make([]byte, 0, n)
    m := n
    return func(item byte) []byte {
        if len(s) < n {
            s = append(s, item)
        } else {
            m++
            if rand.Intn(m) < n {
                s[rand.Intn(n)] = item
            }
        }
        return s
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    var freq [10]int
    for r := 0; r < 1e5; r++ {
        sOfN := sOfNCreator(3)
        for d := byte('0'); d < '9'; d++ {
            sOfN(d)
        }
        for _, d := range sOfN('9') {
            freq[d-'0']++
        }
    }
    fmt.Println(freq)
}
