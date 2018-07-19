package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type Node struct {
    Name     string
    Children []*Node
}

func main() {
    tree := &Node{"root", []*Node{
        &Node{"a", []*Node{
            &Node{"d", nil},
            &Node{"e", []*Node{
                &Node{"f", nil},
            }}}},
        &Node{"b", nil},
        &Node{"c", nil},
    }}
    b, err := json.MarshalIndent(tree, "", "   ")
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(b))
}
