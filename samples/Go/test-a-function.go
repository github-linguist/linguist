package pal

import "testing"

func TestPals(t *testing.T) {
    pals := []string{
        "",
        ".",
        "11",
        "ere",
        "ingirumimusnocteetconsumimurigni",
    }
    for _, s := range pals {
        if !IsPal(s) {
            t.Error("IsPal returned false on palindrome,", s)
        }
    }
}

func TestNonPals(t *testing.T) {
    nps := []string{
        "no",
        "odd",
        "salàlas",
    }
    for _, s := range nps {
        if IsPal(s) {
            t.Error("IsPal returned true on non-palindrome,", s)
        }
    }
}
