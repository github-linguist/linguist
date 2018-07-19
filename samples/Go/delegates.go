package main
import "fmt"

type Delegator struct {
    delegate interface{} // the delegate may be any type
}

// interface that represents anything that supports thing()
type Thingable interface {
    thing() string
}

func (self Delegator) operation() string {
    if v, ok := self.delegate.(Thingable); ok {
        return v.thing()
    }
    return "default implementation"
}

type Delegate int // any dummy type

func (Delegate) thing() string {
    return "delegate implementation"
}

func main() {
    // Without a delegate:
    a := Delegator{}
    fmt.Println(a.operation()) // prints "default implementation"

    // With a delegate that does not implement "thing"
    a.delegate = "A delegate may be any object"
    fmt.Println(a.operation()) // prints "default implementation"

    // With a delegate:
    var d Delegate
    a.delegate = d
    fmt.Println(a.operation()) // prints "delegate implementation"
}
