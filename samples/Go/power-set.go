package main

import (
    "fmt"
    "strconv"
)

// types needed to implement general purpose sets are element and set

// element is an interface, allowing different kinds of elements to be
// implemented and stored in sets.
type element interface {
    // an element must be distinguishable from other elements to satisfy
    // the mathematical definition of a set.  a.eq(b) must give the same
    // result as b.eq(a).
    eq(element) bool
    // String result is used only for printable output.  Given a, b where
    // a.eq(b), it is not required that a.String() == b.String().
    String() string
}

// integer type satisfying element interface
type intEle int

func (i intEle) eq(e element) bool {
    if j, ok := e.(intEle); ok {
        return i == j
    }
    return false
}

func (i intEle) String() string {
    return strconv.Itoa(int(i))
}

// set type implemented as a simple list.  methods will be added to
// make it satisfy the element interface, allowing sets of sets.
type set []element

// uniqueness of elements can be ensured by using add method
func (s *set) addEle(e element) {
    if !s.hasEle(e) {
        *s = append(*s, e)
    }
}

func (s *set) hasEle(e element) bool {
    for _, ex := range *s {
        if e.eq(ex) {
            return true
        }
    }
    return false
}

// method to satify element interface
func (s set) eq(e element) bool {
    t, ok := e.(set)
    if !ok {
        return false
    }
    if len(s) != len(t) {
        return false
    }
    for _, se := range s {
        if !t.hasEle(se) {
            return false
        }
    }
    return true
}

// method to satify element interface
func (s set) String() string {
    r := "{"
    for _, e := range s {
        if len(r) > 1 {
            r += " "
        }
        r += fmt.Sprint(e)
    }
    return r + "}"
}

// method required for task
func (s set) powerSet() set {
    r := set{set{}}
    for _, es := range s {
        var u set
        for _, er := range r {
            u = append(u, append(er.(set), es))
        }
        r = append(r, u...)
    }
    return r
}

func main() {
    var s set
    for _, i := range []intEle{1, 2, 2, 3, 4, 4, 4} {
        s.addEle(i)
    }
    fmt.Println(s)
    fmt.Println("length =", len(s))
    ps := s.powerSet()
    fmt.Println(ps)
    fmt.Println("length =", len(ps))
}
