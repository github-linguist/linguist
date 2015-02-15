package main

import "fmt"

// If your numbers happen to be in the range of Unicode code points (0 to 0x10ffff), this function
// satisfies the task:
func lessRune(a, b []rune) bool {
    return string(a) < string(b) // see also bytes.Compare
}

// Otherwise, the following function satisfies the task for all integer
// and floating point types, by changing the type definition appropriately.
type numericType int

func lessNT(a, b []numericType) bool {
    l := len(a)
    if len(b) < l {
        l = len(b)
    }
    for i := 0; i < l; i++ {
        if a[i] != b[i] {
            return a[i] < b[i]
        }
    }
    return l < len(b)
}

var testCases = [][][]numericType{
    {{0}, {}},
    {{}, {}},
    {{}, {0}},

    {{-1}, {0}},
    {{0}, {0}},
    {{0}, {-1}},

    {{0}, {0, -1}},
    {{0}, {0, 0}},
    {{0}, {0, 1}},
    {{0, -1}, {0}},
    {{0, 0}, {0}},
    {{0, 0}, {1}},
}

func main() {
    // demonstrate the general function
    for _, tc := range testCases {
        fmt.Printf("order %6s before %6s : %t\n",
            fmt.Sprintf("%v", tc[0]),
            fmt.Sprintf("%v", tc[1]),
            lessNT(tc[0], tc[1]))
    }
    fmt.Println()

    // demonstrate that the byte specific function gives identical results
    // by offsetting test data to a printable range of characters.
    for _, tc := range testCases {
        a := toByte(tc[0])
        b := toByte(tc[1])
        fmt.Printf("order %6q before %6q : %t\n",
            string(a),
            string(b),
            lessByte(a, b))
    }
}

func toByte(a []numericType) []byte {
    b := make([]byte, len(a))
    for i, n := range a {
        b[i] = 'b' + byte(n)
    }
    return b
}
