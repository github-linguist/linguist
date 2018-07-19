package main

import "fmt"

// Asymetry in the algorithm suggests different data structures for the
// map value types of the proposers and the recipients.  Proposers go down
// their list of preferences in order, and do not need random access.
// Recipients on the other hand must compare their preferences to arbitrary
// proposers.  A slice is adequate for proposers, but a map allows direct
// lookups for recipients and avoids looping code.
type proposers map[string][]string

var mPref = proposers{
    "abe": []string{
        "abi", "eve", "cath", "ivy", "jan",
        "dee", "fay", "bea", "hope", "gay"},
    "bob": []string{
        "cath", "hope", "abi", "dee", "eve",
        "fay", "bea", "jan", "ivy", "gay"},
    "col": []string{
        "hope", "eve", "abi", "dee", "bea",
        "fay", "ivy", "gay", "cath", "jan"},
    "dan": []string{
        "ivy", "fay", "dee", "gay", "hope",
        "eve", "jan", "bea", "cath", "abi"},
    "ed": []string{
        "jan", "dee", "bea", "cath", "fay",
        "eve", "abi", "ivy", "hope", "gay"},
    "fred": []string{
        "bea", "abi", "dee", "gay", "eve",
        "ivy", "cath", "jan", "hope", "fay"},
    "gav": []string{
        "gay", "eve", "ivy", "bea", "cath",
        "abi", "dee", "hope", "jan", "fay"},
    "hal": []string{
        "abi", "eve", "hope", "fay", "ivy",
        "cath", "jan", "bea", "gay", "dee"},
    "ian": []string{
        "hope", "cath", "dee", "gay", "bea",
        "abi", "fay", "ivy", "jan", "eve"},
    "jon": []string{
        "abi", "fay", "jan", "gay", "eve",
        "bea", "dee", "cath", "ivy", "hope"},
}

type recipients map[string]map[string]int

var wPref = recipients{
    "abi": map[string]int{
        "bob": 1, "fred": 2, "jon": 3, "gav": 4, "ian": 5,
        "abe": 6, "dan": 7, "ed": 8, "col": 9, "hal": 10},
    "bea": map[string]int{
        "bob": 1, "abe": 2, "col": 3, "fred": 4, "gav": 5,
        "dan": 6, "ian": 7, "ed": 8, "jon": 9, "hal": 10},
    "cath": map[string]int{
        "fred": 1, "bob": 2, "ed": 3, "gav": 4, "hal": 5,
        "col": 6, "ian": 7, "abe": 8, "dan": 9, "jon": 10},
    "dee": map[string]int{
        "fred": 1, "jon": 2, "col": 3, "abe": 4, "ian": 5,
        "hal": 6, "gav": 7, "dan": 8, "bob": 9, "ed": 10},
    "eve": map[string]int{
        "jon": 1, "hal": 2, "fred": 3, "dan": 4, "abe": 5,
        "gav": 6, "col": 7, "ed": 8, "ian": 9, "bob": 10},
    "fay": map[string]int{
        "bob": 1, "abe": 2, "ed": 3, "ian": 4, "jon": 5,
        "dan": 6, "fred": 7, "gav": 8, "col": 9, "hal": 10},
    "gay": map[string]int{
        "jon": 1, "gav": 2, "hal": 3, "fred": 4, "bob": 5,
        "abe": 6, "col": 7, "ed": 8, "dan": 9, "ian": 10},
    "hope": map[string]int{
        "gav": 1, "jon": 2, "bob": 3, "abe": 4, "ian": 5,
        "dan": 6, "hal": 7, "ed": 8, "col": 9, "fred": 10},
    "ivy": map[string]int{
        "ian": 1, "col": 2, "hal": 3, "gav": 4, "fred": 5,
        "bob": 6, "abe": 7, "ed": 8, "jon": 9, "dan": 10},
    "jan": map[string]int{
        "ed": 1, "hal": 2, "gav": 3, "abe": 4, "bob": 5,
        "jon": 6, "col": 7, "ian": 8, "fred": 9, "dan": 10},
}

func main() {
    // get parings by Gale/Shapley algorithm
    ps := pair(mPref, wPref)
    // show results
    fmt.Println("\nresult:")
    if !validateStable(ps, mPref, wPref) {
        return
    }
    // perturb
    for {
        i := 0
        var w2, m2 [2]string
        for w, m := range ps {
            w2[i] = w
            m2[i] = m
            if i == 1 {
                break
            }
            i++
        }
        fmt.Println("\nexchanging partners of", m2[0], "and", m2[1])
        ps[w2[0]] = m2[1]
        ps[w2[1]] = m2[0]
        // validate perturbed parings
        if !validateStable(ps, mPref, wPref) {
            return
        }
        // if those happened to be stable as well, perturb more
    }
}

type parings map[string]string // map[recipient]proposer (or map[w]m)

// Pair implements the Gale/Shapley algorithm.
func pair(pPref proposers, rPref recipients) parings {
    // code is destructive on the maps, so work with copies
    pFree := proposers{}
    for k, v := range pPref {
        pFree[k] = append([]string{}, v...)
    }
    rFree := recipients{}
    for k, v := range rPref {
        rFree[k] = v
    }
    // struct only used in this function.
    // preferences must be saved in case engagement is broken.
    type save struct {
        proposer string
        pPref    []string
        rPref    map[string]int
    }
    proposals := map[string]save{} // key is recipient (w)

    // WP pseudocode comments prefaced with WP: m is proposer, w is recipient.
    // WP: while ∃ free man m who still has a woman w to propose to
    for len(pFree) > 0 { // while there is a free proposer,
        var proposer string
        var ppref []string
        for proposer, ppref = range pFree {
            break // pick a proposer at random, whatever range delivers first.
        }
        if len(ppref) == 0 {
            continue // if proposer has no possible recipients, skip
        }
        // WP: w = m's highest ranked such woman to whom he has not yet proposed
        recipient := ppref[0] // highest ranged is first in list.
        ppref = ppref[1:]     // pop from list
        var rpref map[string]int
        var ok bool
        // WP: if w is free
        if rpref, ok = rFree[recipient]; ok {
            // WP: (m, w) become engaged
            // (common code follows if statement)
        } else {
            // WP: else some pair (m', w) already exists
            s := proposals[recipient] // get proposal saved preferences
            // WP: if w prefers m to m'
            if s.rPref[proposer] < s.rPref[s.proposer] {
                fmt.Println("engagement broken:", recipient, s.proposer)
                // WP: m' becomes free
                pFree[s.proposer] = s.pPref // return proposer to the map
                // WP: (m, w) become engaged
                rpref = s.rPref
                // (common code follows if statement)
            } else {
                // WP: else (m', w) remain engaged
                pFree[proposer] = ppref // update preferences in map
                continue
            }
        }
        fmt.Println("engagement:", recipient, proposer)
        proposals[recipient] = save{proposer, ppref, rpref}
        delete(pFree, proposer)
        delete(rFree, recipient)
    }
    // construct return value
    ps := parings{}
    for recipient, s := range proposals {
        ps[recipient] = s.proposer
    }
    return ps
}

func validateStable(ps parings, pPref proposers, rPref recipients) bool {
    for r, p := range ps {
        fmt.Println(r, p)
    }
    for r, p := range ps {
        for _, rp := range pPref[p] {
            if rp == r {
                break
            }
            rprefs := rPref[rp]
            if rprefs[p] < rprefs[ps[rp]] {
                fmt.Println("unstable.")
                fmt.Printf("%s and %s would prefer each other over"+
                    " their current pairings.\n", p, rp)
                return false
            }
        }
    }
    fmt.Println("stable.")
    return true
}
