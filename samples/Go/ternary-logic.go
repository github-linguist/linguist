package main

import "fmt"

type trit int8

const (
    trFalse = iota - 1
    trMaybe
    trTrue
)

func (t trit) String() string {
    switch t {
    case trFalse:
        return "False"
    case trMaybe:
        return "Maybe"
    case trTrue:
        return "True "
    }
    panic("Invalid trit")
}

func trNot(t trit) trit {
    return -t
}

func trAnd(s, t trit) trit {
    if s < t {
        return s
    }
    return t
}

func trOr(s, t trit) trit {
    if s > t {
        return s
    }
    return t
}

func trEq(s, t trit) trit {
    return s * t
}

func main() {
    trSet := []trit{trFalse, trMaybe, trTrue}

    fmt.Println("t     not t")
    for _, t := range trSet {
        fmt.Println(t, trNot(t))
    }

    fmt.Println("\ns     t     s and t")
    for _, s := range trSet {
        for _, t := range trSet {
            fmt.Println(s, t, trAnd(s, t))
        }
    }

    fmt.Println("\ns     t     s or t")
    for _, s := range trSet {
        for _, t := range trSet {
            fmt.Println(s, t, trOr(s, t))
        }
    }

    fmt.Println("\ns     t     s eq t")
    for _, s := range trSet {
        for _, t := range trSet {
            fmt.Println(s, t, trEq(s, t))
        }
    }
}
