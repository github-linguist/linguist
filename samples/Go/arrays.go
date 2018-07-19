package main

import (
    "fmt"
)

func main() {
    // creates an array of five ints.
    // specified length must be a compile-time constant expression.
    // this allows compiler to do efficient bounds checking.
    var a [5]int

    // since length is compile-time constant, len() is a compile time constant
    // and does not have the overhead of a function call.
    fmt.Println("len(a) =", len(a))

    // elements are always initialized to 0
    fmt.Println("a =", a)

    // assign a value to an element.  indexing is 0 based.
    a[0] = 3
    fmt.Println("a =", a)

    // retrieve element value with same syntax
    fmt.Println("a[0] =", a[0])

    // a slice references an underlying array
    s := a[:4] // this does not allocate new array space.
    fmt.Println("s =", s)

    // slices have runtime established length and capacity, but len() and
    // cap() are built in to the compiler and have overhead more like
    // variable access than function call.
    fmt.Println("len(s) =", len(s), " cap(s) =", cap(s))

    // slices can be resliced, as long as there is space
    // in the underlying array.
    s = s[:5]
    fmt.Println("s =", s)

    // s still based on a
    a[0] = 22
    fmt.Println("a =", a)
    fmt.Println("s =", s)

    // append will automatically allocate a larger underlying array as needed.
    s = append(s, 4, 5, 6)
    fmt.Println("s =", s)
    fmt.Println("len(s) =", len(s), " cap(s) =", cap(s))

    // s no longer based on a
    a[4] = -1
    fmt.Println("a =", a)
    fmt.Println("s =", s)

    // make creates a slice and allocates a new underlying array
    s = make([]int, 8)
    fmt.Println("s =", s)
    fmt.Println("len(s) =", len(s), " cap(s) =", cap(s))

    // the cap()=10 array is no longer referenced
    // and would be garbage collected eventually.
}
