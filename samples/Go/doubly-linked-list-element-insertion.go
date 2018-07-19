package main

import "fmt"

type dlNode struct {
    string
    next, prev *dlNode
}

type dlList struct {
    head, tail *dlNode
}

func (list *dlList) String() string {
    if list.head == nil {
        return fmt.Sprint(list.head)
    }
    r := "[" + list.head.string
    for p := list.head.next; p != nil; p = p.next {
        r += " " + p.string
    }
    return r + "]"
}

func (list *dlList) insertTail(node *dlNode) {
    if list.tail == nil {
        list.head = node
    } else {
        list.tail.next = node
    }
    node.next = nil
    node.prev = list.tail
    list.tail = node
}

func (list *dlList) insertAfter(existing, insert *dlNode) {
    insert.prev = existing
    insert.next = existing.next
    existing.next.prev = insert
    existing.next = insert
    if existing == list.tail {
        list.tail = insert
    }
}

func main() {
    dll := &dlList{}
    fmt.Println(dll)
    a := &dlNode{string: "A"}
    dll.insertTail(a)
    dll.insertTail(&dlNode{string: "B"})
    fmt.Println(dll)
    dll.insertAfter(a, &dlNode{string: "C"})
    fmt.Println(dll)
}
