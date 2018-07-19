package main

import "fmt"

type set map[string]bool

var testCase = []set{
    set{"H": true, "I": true, "K": true},
    set{"A": true, "B": true},
    set{"C": true, "D": true},
    set{"D": true, "B": true},
    set{"F": true, "G": true, "H": true},
}

func main() {
    fmt.Println(consolidate(testCase))
}

func consolidate(sets []set) []set {
    setlist := []set{}
    for _, s := range sets {
        if s != nil && len(s) > 0 {
            setlist = append(setlist, s)
        }
    }
    for i, s1 := range setlist {
        if len(s1) > 0 {
            for _, s2 := range setlist[i+1:] {
                if s1.disjoint(s2) {
                    continue
                }
                for e := range s1 {
                    s2[e] = true
                    delete(s1, e)
                }
                s1 = s2
            }
        }
    }
    r := []set{}
    for _, s := range setlist {
        if len(s) > 0 {
            r = append(r, s)
        }
    }
    return r
}

func (s1 set) disjoint(s2 set) bool {
    for e := range s2 {
        if s1[e] {
            return false
        }
    }
    return true
}
