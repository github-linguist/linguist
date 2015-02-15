package main

import (
    "fmt"
    "reflect"
)

type example struct{}

// the method must be exported to be accessed through reflection.
func (example) Foo() int {
    return 42
}

func main() {
    // create an object with a method
    var e example
    // get the method by name
    m := reflect.ValueOf(e).MethodByName("Foo")
    // call the method with no argments
    r := m.Call(nil)
    // interpret first return value as int
    fmt.Println(r[0].Int()) // => 42
}
