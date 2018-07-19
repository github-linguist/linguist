package main

import "fmt"

var (
    s []int
    p *int
    f func()
    i interface{}
    m map[int]int
    c chan int
)

func main() {
    fmt.Println("Exercise nil objects:")
    status()

    // initialize objects
    s = make([]int, 1)
    p = &s[0] // yes, reference element of slice just created
    f = func() { fmt.Println("function call") }
    i = user(0) // see user defined type just below
    m = make(map[int]int)
    c = make(chan int, 1)

    fmt.Println("\nExercise objects after initialization:")
    status()
}

type user int

func (user) m() {
    fmt.Println("method call")
}

func status() {
    trySlice()
    tryPointer()
    tryFunction()
    tryInterface()
    tryMap()
    tryChannel()
}

func reportPanic() {
    if x := recover(); x != nil {
        fmt.Println("panic:", x)
    }
}

func trySlice() {
    defer reportPanic()
    fmt.Println("s[0] =", s[0])
}

func tryPointer() {
    defer reportPanic()
    fmt.Println("*p =", *p)
}

func tryFunction() {
    defer reportPanic()
    f()
}

func tryInterface() {
    defer reportPanic()

    // normally the nil identifier accesses a nil value for one of
    // six predefined types.  In a type switch however, nil can be used
    // as a type.  In this case, it matches the nil interface.
    switch i.(type) {
    case nil:
        fmt.Println("i is nil interface")
    case interface {
        m()
    }:
        fmt.Println("i has method m")
    }

    // assert type with method and then call method
    i.(interface {
        m()
    }).m()
}

func tryMap() {
    defer reportPanic()
    m[0] = 0
    fmt.Println("m[0] =", m[0])
}

func tryChannel() {
    defer reportPanic()
    close(c)
    fmt.Println("channel closed")
}
