// A fairly literal translation of the example program on the referenced
// WP page.  Well, it happened to be the example program the day I completed
// the task.  It seems from the WP history that there has been some churn
// in the posted example program.  The example program of the day was in
// Pascal and was credited to Niklaus Wirth, from his "Algorithms +
// Data Structures = Programs."
package main

import "fmt"

var (
    i int
    q bool
    a [9]bool
    b [17]bool
    c [15]bool // offset by 7 relative to the Pascal version
    x [9]int
)

func try(i int) {
    for j := 1; ; j++ {
        q = false
        if a[j] && b[i+j] && c[i-j+7] {
            x[i] = j
            a[j] = false
            b[i+j] = false
            c[i-j+7] = false
            if i < 8 {
                try(i + 1)
                if !q {
                    a[j] = true
                    b[i+j] = true
                    c[i-j+7] = true
                }
            } else {
                q = true
            }
        }
        if q || j == 8 {
            break
        }
    }
}

func main() {
    for i := 1; i <= 8; i++ {
        a[i] = true
    }
    for i := 2; i <= 16; i++ {
        b[i] = true
    }
    for i := 0; i <= 14; i++ {
        c[i] = true
    }
    try(1)
    if q {
        for i := 1; i <= 8; i++ {
            fmt.Println(i, x[i])
        }
    }
}
