// Outline for a try/catch-like exception mechanism in Go
//
// As all Go programmers should know, the Go authors are sharply critical of
// the try/catch idiom and consider it bad practice in general.
// See http://golang.org/doc/go_faq.html#exceptions

package main

import (
    "fmt"
    "runtime"
    "strings"
)

// trace is for pretty output for the Rosetta Code task.
// It would have no place in a practical program.
func trace(s string) {
    nc := runtime.Callers(2, cs)
    f := runtime.FuncForPC(cs[0])
    fmt.Print(strings.Repeat("  ", nc-3), f.Name()[5:], ": ", s, "\n")
}

var cs = make([]uintptr, 10)

type exception struct {
    name    string
    handler func()
}

// try implents the try/catch-like exception mechanism.  It takes a function
// to be called, and a list of exceptions to catch during the function call.
// Note that for this simple example, f has no parameters.  In a practical
// program it might, of course.  In this case, the signature of try would
// have to be modified to take these parameters and then supply them to f
// when it calls f.
func try(f func(), exs []exception) {
    trace("start")
    defer func() {
        if pv := recover(); pv != nil {
            trace("Panic mode!")
            if px, ok := pv.(exception); ok {
                for _, ex := range exs {
                    if ex.name == px.name {
                        trace("handling exception")
                        px.handler()
                        trace("panic over")
                        return
                    }
                }
            }
            trace("can't recover this one!")
            panic(pv)
        }
    }()
    f()
    trace("complete")
}

func main() {
    trace("start")
    foo()
    trace("complete")
}

// u0, u1 declared at package level so they can be accessed by any function.
var u0, u1 exception

// foo.  Note that function literals u0, u1 here in the lexical scope
// of foo serve the purpose of catch blocks of other languages.
// Passing u0 to try serves the purpose of the catch condition.
// While try(bar... reads much like the try statement of other languages,
// this try is an ordinary function.  foo is passing bar into try,
// not calling it directly.
func foo() {
    trace("start")
    u0 = exception{"U0", func() { trace("U0 handled") }}
    u1 = exception{"U1", func() { trace("U1 handled") }}
    try(bar, []exception{u0})
    try(bar, []exception{u0})
    trace("complete")
}

func bar() {
    trace("start")
    baz()
    trace("complete")
}

var bazCall int

func baz() {
    trace("start")
    bazCall++
    switch bazCall {
    case 1:
        trace("panicking with execption U0")
        panic(u0)
    case 2:
        trace("panicking with execption U1")
        panic(u1)
    }
    trace("complete")
}
