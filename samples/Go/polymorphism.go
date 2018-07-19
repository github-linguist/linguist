package main

import "fmt"

type point struct {
    x, y float64
}

type circle struct {
    x, y, r float64
}

type printer interface {
    print()
}

func (p *point) print() {
    fmt.Println(p.x, p.y)
}

func (c *circle) print() {
    fmt.Println(c.x, c.y, c.r)
}

func main() {
    var i printer            // polymorphic variable
    i = newPoint(3, 4)       // assign one type
    i.print()                // call polymorphic function
    i = newCircle(5, 12, 13) // assign different type to same variable
    i.print()                // same call accesses different method now.
}

// Above is a sort of polymorphism: both types implement the printer
// interface.  The print function can be called through a variable
// of type printer, without knowing the underlying type.

// Below is other stuff the task asks for.  Note that none of it is
// needed for cases as simple as this task, and it is not idomatic
// to write any of these functions in these simple cases.

// Accessors are not idiomatic in Go.  Instead, simply access struct
// fields directly.  To allow access from another package, you "export"
// the field by capitalizing the field name.
func (p *point) getX() float64  { return p.x }
func (p *point) getY() float64  { return p.y }
func (p *point) setX(v float64) { p.x = v }
func (p *point) setY(v float64) { p.y = v }

func (c *circle) getX() float64  { return c.x }
func (c *circle) getY() float64  { return c.y }
func (c *circle) getR() float64  { return c.r }
func (c *circle) setX(v float64) { c.x = v }
func (c *circle) setY(v float64) { c.y = v }
func (c *circle) setR(v float64) { c.r = v }

// Copy constructors, not idiomatic.  Structs are assignable so
// you can simply declare and assign them as needed.
func (p *point) clone() *point   { r := *p; return &r }
func (c *circle) clone() *circle { r := *c; return &r }

// Assignment methods, not idiomatic.  Just use the assignment operator.
func (p *point) set(q *point)   { *p = *q }
func (c *circle) set(d *circle) { *c = *d }

// Constructors are idiomatic only when construction involves something
// more than just assigning initial values.  By default, structs
// are created as "zero values," that is, with all fields zero,
// empty, or nil.  The struct literal synax allows for all fields to
// initialized, or for any subset of fields to be initialized by name.
// These feautures take the place of trivial default constructors.
// When additional initialization is needed, it is conventional to
// name a function New, New<Type>, or within a package, new<Type>
// as shown here.
func newPoint(x, y float64) *point {
    return &point{x, y}
}
func newCircle(x, y, r float64) *circle {
    return &circle{x, y, r}
}

// Destructors are never used in Go.  Objects are garbage collected.
