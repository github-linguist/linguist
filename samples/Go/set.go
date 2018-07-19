package main

import "fmt"

type set map[int]bool

func main() {
    // task: set creation
    s0 := make(set)             // create empty set
    s1 := set{3: true}          // create set with one element
    s2 := set{3: true, 1: true} // create set with two elements

    // option: another way to create a set
    s3 := newSet([]int{3, 1, 4, 1, 5, 9})

    // option: output!
    fmt.Println("s0:", s0)
    fmt.Println("s1:", s1)
    fmt.Println("s2:", s2)
    fmt.Println("s3:", s3)

    // task: element predicate
    fmt.Printf("%v ∈ s0: %t\n", 3, s0.hasElement(3))
    fmt.Printf("%v ∈ s3: %t\n", 3, s3.hasElement(3))
    fmt.Printf("%v ∈ s3: %t\n", 2, s3.hasElement(2))

    // task: union
    b := set{4: true, 2: true}
    fmt.Printf("s3 ∪ %v: %v\n", b, union(s3, b))

    // task: intersection
    fmt.Printf("s3 ∩ %v: %v\n", b, intersection(s3, b))

    // task: difference
    fmt.Printf("s3 \\ %v: %v\n", b, difference(s3, b))

    // task: subset predicate
    fmt.Printf("%v ⊆ s3: %t\n", b, subset(b, s3))
    fmt.Printf("%v ⊆ s3: %t\n", s2, subset(s2, s3))
    fmt.Printf("%v ⊆ s3: %t\n", s0, subset(s0, s3))

    // task: equality
    s2Same := set{1: true, 3: true}
    fmt.Printf("%v = s2: %t\n", s2Same, equal(s2Same, s2))

    // option: proper subset
    fmt.Printf("%v ⊂ s2: %t\n", s2Same, properSubset(s2Same, s2))
    fmt.Printf("%v ⊂ s3: %t\n", s2Same, properSubset(s2Same, s3))

    // option: delete.  it's built in.
    delete(s3, 3)
    fmt.Println("s3, 3 deleted:", s3)
}

func newSet(ms []int) set {
    s := make(set)
    for _, m := range ms {
        s[m] = true
    }
    return s
}

func (s set) String() string {
    if len(s) == 0 {
        return "∅"
    }
    r := "{"
    for e := range s {
        r = fmt.Sprintf("%s%v, ", r, e)
    }
    return r[:len(r)-2] + "}"
}

func (s set) hasElement(m int) bool {
    return s[m]
}

func union(a, b set) set {
    s := make(set)
    for e := range a {
        s[e] = true
    }
    for e := range b {
        s[e] = true
    }
    return s
}

func intersection(a, b set) set {
    s := make(set)
    for e := range a {
        if b[e] {
            s[e] = true
        }
    }
    return s
}

func difference(a, b set) set {
    s := make(set)
    for e := range a {
        if !b[e] {
            s[e] = true
        }
    }
    return s
}

func subset(a, b set) bool {
    for e := range a {
        if !b[e] {
            return false
        }
    }
    return true
}

func equal(a, b set) bool {
    return len(a) == len(b) && subset(a, b)
}

func properSubset(a, b set) bool {
    return len(a) < len(b) && subset(a, b)
}
