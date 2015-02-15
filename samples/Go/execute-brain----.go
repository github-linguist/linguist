package main

import "fmt"

func main() {
    // example program is current Brain**** solution to
    // Hello world/Text task.  only requires 10 bytes of data store!
    bf(10, `++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++
++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>
>+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.
<+++++++.--------.<<<<<+.<+++.---.`)
}

func bf(dLen int, is string) {
    ds := make([]byte, dLen) // data store
    var dp int               // data pointer
    for ip := 0; ip < len(is); ip++ {
        switch is[ip] {
        case '>':
            dp++
        case '<':
            dp--
        case '+':
            ds[dp]++
        case '-':
            ds[dp]--
        case '.':
            fmt.Printf("%c", ds[dp])
        case ',':
            fmt.Scanf("%c", &ds[dp])
        case '[':
            if ds[dp] == 0 {
                for nc := 1; nc > 0; {
                    ip++
                    if is[ip] == '[' {
                        nc++
                    } else if is[ip] == ']' {
                        nc--
                    }
                }
            }
        case ']':
            if ds[dp] != 0 {
                for nc := 1; nc > 0; {
                    ip--
                    if is[ip] == ']' {
                        nc++
                    } else if is[ip] == '[' {
                        nc--
                    }
                }
            }
        }
    }
}
