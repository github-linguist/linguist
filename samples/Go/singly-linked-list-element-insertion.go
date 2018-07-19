package main

import "fmt"

type Ele struct {
    Data interface{}
    Next *Ele
}

func (e *Ele) insert(data interface{}) {
    if e == nil {
        panic("attept to modify nil")
    }
    e.Next = &Ele{data, e.Next}
}

func (e *Ele) printList() {
    if e == nil {
        fmt.Println(nil)
        return
    }
    fmt.Printf("(%v", e.Data)
    for {
        e = e.Next
        if e == nil {
            fmt.Println(")")
            return
        }
        fmt.Print(" ", e.Data)
    }
}

func main() {
    h := &Ele{"A", &Ele{"B", nil}}
    h.printList()
    h.insert("C")
    h.printList()
}
