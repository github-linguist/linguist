package main

import "fmt"

type node struct {
    int
    left, right *node
}

// function returns a channel that yields the leaves of the tree.
// the channel is closed after all leaves are received.
func leaves(t *node) chan int {
    ch := make(chan int)
    // recursive function to walk tree.
    var f func(*node)
    f = func(n *node) {
        if n == nil {
            return
        }
        // leaves are identified by having no children.
        if n.left == nil && n.right == nil {
            ch <- n.int
        } else {
            f(n.left)
            f(n.right)
        }
    }
    // goroutine runs concurrently with others.
    // it walks the tree then closes the channel.
    go func() {
        f(t)
        close(ch)
    }()
    return ch
}

func sameFringe(t1, t2 *node) bool {
    f1 := leaves(t1)
    f2 := leaves(t2)
    for l1 := range f1 {
        // both trees must yield a leaf, and the leaves must be equal.
        if l2, ok := <-f2; !ok || l1 != l2 {
            return false
        }
    }
    // there must be nothing left in f2 after consuming all of f1.
    _, ok := <-f2
    return !ok
}

func main() {
    // the different shapes of the trees is shown with indention.
    // the leaves are easy to spot by the int: key.
    t1 := &node{3,
        &node{1,
            &node{int: 1},
            &node{int: 2}},
        &node{8,
            &node{int: 5},
            &node{int: 13}}}
    // t2 with negative values for internal nodes that can't possibly match
    // positive values in t1, just to show that only leaves are being compared.
    t2 := &node{-8,
        &node{-3,
            &node{-1,
                &node{int: 1},
                &node{int: 2}},
            &node{int: 5}},
        &node{int: 13}}
    fmt.Println(sameFringe(t1, t2)) // prints true.
}
