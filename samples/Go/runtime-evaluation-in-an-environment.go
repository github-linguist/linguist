package main

import (
    "bitbucket.org/binet/go-eval/pkg/eval"
    "fmt"
    "go/parser"
    "go/token"
)

func main() {
    // an expression on x
    squareExpr := "x*x"

    // parse to abstract syntax tree
    fset := token.NewFileSet()
    squareAst, err := parser.ParseExpr(squareExpr)
    if err != nil {
        fmt.Println(err)
        return
    }
    // create an environment or "world"
    w := eval.NewWorld()

    // allocate a variable
    wVar := new(intV)

    // bind the variable to the name x
    err = w.DefineVar("x", eval.IntType, wVar)
    if err != nil {
        fmt.Println(err)
        return
    }
    // bind the expression AST to the world
    squareCode, err := w.CompileExpr(fset, squareAst)
    if err != nil {
        fmt.Println(err)
        return
    }
    // directly manipulate value of variable within world
    *wVar = 5
    // evaluate
    r0, err := squareCode.Run()
    if err != nil {
        fmt.Println(err)
        return
    }
    // change value
    *wVar--
    // revaluate
    r1, err := squareCode.Run()
    if err != nil {
        fmt.Println(err)
        return
    }
    // print difference
    fmt.Println(r0.(eval.IntValue).Get(nil) - r1.(eval.IntValue).Get(nil))
}

// int value implementation.
type intV int64

func (v *intV) String() string              { return fmt.Sprint(*v) }
func (v *intV) Get(*eval.Thread) int64      { return int64(*v) }
func (v *intV) Set(_ *eval.Thread, x int64) { *v = intV(x) }
func (v *intV) Assign(t *eval.Thread, o eval.Value) {
    *v = intV(o.(eval.IntValue).Get(t))
}
