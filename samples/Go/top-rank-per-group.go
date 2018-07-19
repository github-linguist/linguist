package main

import (
    "fmt"
    "sort"
)

// language-native data description
type employee struct {
    name, id string
    salary   int
    dept     string
}

var data = []employee{
    {"Tyler Bennett", "E10297", 32000, "D101"},
    {"John Rappl", "E21437", 47000, "D050"},
    {"George Woltman", "E00127", 53500, "D101"},
    {"Adam Smith", "E63535", 18000, "D202"},
    {"Claire Buckman", "E39876", 27800, "D202"},
    {"David McClellan", "E04242", 41500, "D101"},
    {"Rich Holcomb", "E01234", 49500, "D202"},
    {"Nathan Adams", "E41298", 21900, "D050"},
    {"Richard Potter", "E43128", 15900, "D101"},
    {"David Motsinger", "E27002", 19250, "D202"},
    {"Tim Sampair", "E03033", 27000, "D101"},
    {"Kim Arlich", "E10001", 57000, "D190"},
    {"Timothy Grove", "E16398", 29900, "D190"},
}

// n provided as first parameter of function topN
func main() {
    topN(3, data).print()

}

// return type of function topN
type result struct {
    n     int
    dlist [][][]int // dimensions are department,salary,index into data
}

func (r result) print() {
    fmt.Println("Top", r.n, "salaries per department")
    printEs := func(es []int) {
        for _, ei := range es {
            fmt.Printf("    %s %16s %7d\n",
                data[ei].id, data[ei].name, data[ei].salary)
        }
    }
    for _, slist := range r.dlist {
        fmt.Println(data[slist[0][0]].dept)
        nRemaining := r.n
        sLast := len(slist) - 1
        for si := 0; si < sLast; si++ {
            printEs(slist[si])
            nRemaining -= len(slist[si])
        }
        if len(slist[sLast]) > nRemaining {
            fmt.Println("    * * * * * * * tie * * * * * * *")
        }
        printEs(slist[sLast])
    }
}

func topN(n int, data []employee) *result {
    // summarize:  construct map with unique deptartment names,
    // unique salaries under each of those,
    // and then a list of employees under each of those,
    // with employees identified by index into data.
    salsum := make(map[string]map[int][]int)
    for i, e := range data {
        dsum, ok := salsum[e.dept]
        if !ok {
            dsum = make(map[int][]int)
            salsum[e.dept] = dsum
        }
        dsum[e.salary] = append(dsum[e.salary], i)
    }

    // begin constructing result array.
    // top level is list of departments.
    dlist := make([][][]int, len(salsum))

    // sort departments for readability
    deptSort := make([]string, len(salsum))
    i := 0
    for dept := range salsum {
        deptSort[i] = dept
        i++
    }
    sort.Strings(deptSort)

    for di, dept := range deptSort {
        dsum := salsum[dept]
        // sort salaries for selection (and readability)
        salSort := make([]int, len(dsum))
        i := 0
        for s := range dsum {
            salSort[i] = s
            i++
        }
        sort.Ints(salSort)
        nRemaining := n
        // construct level 2, unique salaries
        var slist [][]int
        // iterate from end so as to select highest
        for si := len(salSort) - 1; si >= 0; si-- {
            ssum := dsum[salSort[si]]
            // construct level 3, indexes into data
            elist := make([]int, len(ssum))
            slist = append(slist, elist)
            copy(elist, ssum)
            nRemaining -= len(ssum)
            if nRemaining <= 0 {
                break
            }
        }
        dlist[di] = slist
    }
    return &result{n, dlist}
}
