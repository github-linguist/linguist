package main

import "fmt"

func average(c intCollection) float64 {
    var sum, count int
    c.mapElements(func(n int) {
        sum += n
        count++
    })
    return float64(sum) / float64(count)
}

func main() {
    t1 := new(binaryTree)
    t2 := new(bTree)
    a1 := average(t1)
    a2 := average(t2)
    fmt.Println("binary tree average:", a1)
    fmt.Println("b-tree average:", a2)
}

type intCollection interface {
    mapElements(func(int))
}

type binaryTree struct {
    // dummy representation details
    left, right bool
}

func (t *binaryTree) mapElements(visit func(int)) {
    // dummy implementation
    if t.left == t.right {
        visit(3)
        visit(1)
        visit(4)
    }
}

type bTree struct {
    // dummy representation details
    buckets int
}

func (t *bTree) mapElements(visit func(int)) {
    // dummy implementation
    if t.buckets >= 0 {
        visit(1)
        visit(5)
        visit(9)
    }
}
