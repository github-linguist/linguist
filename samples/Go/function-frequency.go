package main

import (
    "fmt"
    "go/ast"
    "go/parser"
    "go/token"
    "io/ioutil"
    "os"
    "sort"
)

func main() {
    if len(os.Args) != 2 {
        fmt.Println("usage ff <go source filename>")
        return
    }
    src, err := ioutil.ReadFile(os.Args[1])
    if err != nil {
        fmt.Println(err)
        return
    }
    fs := token.NewFileSet()
    a, err := parser.ParseFile(fs, os.Args[1], src, 0)
    if err != nil {
        fmt.Println(err)
        return
    }
    f := fs.File(a.Pos())
    m := make(map[string]int)
    ast.Inspect(a, func(n ast.Node) bool {
        if ce, ok := n.(*ast.CallExpr); ok {
            start := f.Offset(ce.Pos())
            end := f.Offset(ce.Lparen)
            m[string(src[start:end])]++
        }
        return true
    })
    cs := make(calls, 0, len(m))
    for k, v := range m {
        cs = append(cs, &call{k, v})
    }
    sort.Sort(cs)
    for i, c := range cs {
        fmt.Printf("%-20s %4d\n", c.expr, c.count)
        if i == 9 {
            break
        }
    }
}

type call struct {
    expr  string
    count int
}
type calls []*call

func (c calls) Len() int           { return len(c) }
func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }
func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }
