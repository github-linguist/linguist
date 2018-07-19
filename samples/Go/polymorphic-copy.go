package main

import (
    "fmt"
    "reflect"
)

// interface types provide polymorphism, but not inheritance.
type i interface {
    identify() string
}

// "base" type
type t float64

// "derived" type.  in Go terminology, it is simply a struct with an
// anonymous field.  fields and methods of anonymous fields however,
// can be accessed without additional qualification and so are
// "inherited" in a sense.
type s struct {
    t
    kōan string
}

// another type with an "embedded" t field.  (t is the *type*, this field
// has no *name*.)
type r struct {
    t
    ch chan int
}

// a method on t.  this method makes t satisfy interface i.
// since a t is embedded in types s and r, they automatically "inherit"
// the method.
func (x t) identify() string {
    return "I'm a t!"
}

// the same method on s.  although s already satisfied i, calls to identify
// will now find this method rather than the one defined on t.  in a sense
// it "overrides" the method of the "base class."
func (x s) identify() string {
    return "I'm an s!"
}

func main() {
    // three variables with different types, initialized from literals.
    var t1 t = 5
    var s1 s = s{6, "one"}
    var r1 r = r{t: 7}

    // variables declared with the same type.  initial value is nil.
    var i1, i2, i3 i
    fmt.Println("Initial (zero) values of interface variables:")
    fmt.Println("i1:", i1)
    fmt.Println("i2:", i2)
    fmt.Println("i3:", i3)

    // in the terminology of the Go language reference, i1, i2, and i3
    // still have static type i, but now have different dynamic types.
    i1, i2, i3 = t1, s1, r1
    fmt.Println("\nPolymorphic:")
    fmt.Println("i1:", i1, "/", i1.identify(), "/", reflect.TypeOf(i1))
    fmt.Println("i2:", i2, "/", i2.identify(), "/", reflect.TypeOf(i2))
    fmt.Println("i3:", i3, "/", i3.identify(), "/", reflect.TypeOf(i3))

    // copy: declare and assign in one step using "short declaration."
    i1c, i2c, i3c := i1, i2, i3

    // modify first set of polymorphic variables.
    i1, i2, i3 = s{3, "dog"}, r{t: 1}, t(2)

    // demonstrate that copies are distinct from first set
    // and that types are preserved.
    fmt.Println("\nFirst set now modified:")
    fmt.Println("i1:", i1, "/", i1.identify(), "/", reflect.TypeOf(i1))
    fmt.Println("i2:", i2, "/", i2.identify(), "/", reflect.TypeOf(i2))
    fmt.Println("i3:", i3, "/", i3.identify(), "/", reflect.TypeOf(i3))

    fmt.Println("\nCopies made before modifications:")
    fmt.Println("i1c:", i1c, "/", i1c.identify(), "/", reflect.TypeOf(i1c))
    fmt.Println("i2c:", i2c, "/", i2c.identify(), "/", reflect.TypeOf(i2c))
    fmt.Println("i3c:", i3c, "/", i3c.identify(), "/", reflect.TypeOf(i3c))
}
