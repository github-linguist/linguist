package main

import (
    "fmt"
    "os"

    "launchpad.net/xmlpath"
)

func main() {
    f, err := os.Open("test3.xml")
    if err != nil {
        fmt.Println(err)
        return
    }
    n, err := xmlpath.Parse(f)
    f.Close()
    if err != nil {
        fmt.Println(err)
        return
    }
    q1 := xmlpath.MustCompile("//item")
    if _, ok := q1.String(n); !ok {
        fmt.Println("no item")
    }
    q2 := xmlpath.MustCompile("//price")
    for it := q2.Iter(n); it.Next(); {
        fmt.Println(it.Node())
    }
    q3 := xmlpath.MustCompile("//name")
    names := []*xmlpath.Node{}
    for it := q3.Iter(n); it.Next(); {
        names = append(names, it.Node())
    }
    if len(names) == 0 {
        fmt.Println("no names")
    }
}
