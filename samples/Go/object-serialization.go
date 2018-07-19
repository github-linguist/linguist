package main

import (
    "encoding/gob"
    "fmt"
    "os"
)

type printable interface {
    print()
}

func main() {
    // create instances
    animals := []printable{
        &Animal{Alive: true},
        &Cat{},
        &Lab{
            Dog:   Dog{Animal: Animal{Alive: true}},
            Color: "yellow",
        },
        &Collie{Dog: Dog{
            Animal:           Animal{Alive: true},
            ObedienceTrained: true,
        }},
    }

    // display
    fmt.Println("created:")
    for _, a := range animals {
        a.print()
    }

    // serialize
    f, err := os.Create("objects.dat")
    if err != nil {
        fmt.Println(err)
        return
    }
    for _, a := range animals {
        gob.Register(a)
    }
    err = gob.NewEncoder(f).Encode(animals)
    if err != nil {
        fmt.Println(err)
        return
    }
    f.Close()

    // read
    f, err = os.Open("objects.dat")
    if err != nil {
        fmt.Println(err)
        return
    }
    var clones []printable
    gob.NewDecoder(f).Decode(&clones)
    if err != nil {
        fmt.Println(err)
        return
    }

    // display
    fmt.Println("\nloaded from objects.dat:")
    for _, c := range clones {
        c.print()
    }
}

type Animal struct {
    Alive bool
}

func (a *Animal) print() {
    if a.Alive {
        fmt.Println("   live animal, unspecified type")
    } else {
        fmt.Println("   dead animal, unspecified type")
    }
}

type Dog struct {
    Animal
    ObedienceTrained bool
}

func (d *Dog) print() {
    switch {
    case !d.Alive:
        fmt.Println("   dead dog")
    case d.ObedienceTrained:
        fmt.Println("   trained dog")
    default:
        fmt.Println("   dog, not trained")
    }
}

type Cat struct {
    Animal
    LitterBoxTrained bool
}

func (c *Cat) print() {
    switch {
    case !c.Alive:
        fmt.Println("   dead cat")
    case c.LitterBoxTrained:
        fmt.Println("   litter box trained cat")
    default:
        fmt.Println("   cat, not litter box trained")
    }
}

type Lab struct {
    Dog
    Color string
}

func (l *Lab) print() {
    var r string
    if l.Color == "" {
        r = "lab, color unspecified"
    } else {
        r = l.Color + " lab"
    }
    switch {
    case !l.Alive:
        fmt.Println("   dead", r)
    case l.ObedienceTrained:
        fmt.Println("   trained", r)
    default:
        fmt.Printf("   %s, not trained\n", r)
    }
}

type Collie struct {
    Dog
    CatchesFrisbee bool
}

func (c *Collie) print() {
    switch {
    case !c.Alive:
        fmt.Println("   dead collie")
    case c.ObedienceTrained && c.CatchesFrisbee:
        fmt.Println("   trained collie, catches frisbee")
    case c.ObedienceTrained && !c.CatchesFrisbee:
        fmt.Println("   trained collie, but doesn't catch frisbee")
    case !c.ObedienceTrained && c.CatchesFrisbee:
        fmt.Println("   collie, not trained, but catches frisbee")
    case !c.ObedienceTrained && !c.CatchesFrisbee:
        fmt.Println("   collie, not trained, doesn't catch frisbee")
    }
}
