package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())
    d := newDeck()
    fmt.Println("fresh deck")
    fmt.Println(d)

    d.shuffle()
    fmt.Println("\nshuffled")
    fmt.Println(d)

    h := d.deal(5)
    fmt.Println("\n5 cards delt")
    fmt.Println(h)

    fmt.Println("\npip, suit values of cards in hand:")
    fmt.Println("pip  suit")
    for _, c := range h {
        fmt.Printf("%3d   %3d\n", c.pip(), c.suit())
    }
    fmt.Print("\n", len(d), " cards left in deck\n")
    fmt.Println(d)
}

// card type.  (deck type defined later)
// valid range is 0..51.  this facilititates computations.
type card int

// zero-based pip/suit functions.  useful for computations.
// (or maybe just reference for computations, since they are trivial)
func (c card) pip0() int {
    return int(c % 13)
}

func (c card) suit0() int {
    return int(c / 13)
}

func (c card) pipSuit0() (int, int) {
    return int(c % 13), int(c / 13)
}

// one-base pip/suit functions.  meaningful for output.
// ("pip" does really mean the identifying number of spots on the card)
func (c card) pip() int {
    return int(c%13) + 1
}

func (c card) suit() int {
    return int(c/13) + 1
}

func (c card) pipSuit() (int, int) {
    return int(c%13) + 1, int(c/13) + 1
}

// string representation
const pipS = "A23456789TJQK"
const suitS = "CDHS"

func (c card) String() string {
    px, sx := c.pipSuit0()
    return pipS[px:px+1] + suitS[sx:sx+1]
}

// deck type
type deck []card

// "constructor" returns pointer to new deck object
func newDeck() deck {
    d := make(deck, 52)
    for i := range d {
      /lang>

Example (first 4 cards of a shuffled deck):
  d[i] = card(i)
    }
    return d
}

// string representation:  13 cards to a line, up to four lines.
// lines separated with newline character.  empty deck is empty string.
func (d deck) String() string {
    if len(d) == 0 {
        return ""
    }
    r := d[0].String()
    for i, c := range d[1:] {
        if i%13 == 12 {
            r += "\n"
        } else {
            r += " "
        }
        r += c.String()
    }
    return r
}

// shuffle in place (although using extra memory)
func (d deck) shuffle() {
    d2 := make(deck, len(d))
    copy(d2, d)
    for i, s := range rand.Perm(len(d)) {
        d[i] = d2[s]
    }
}

// return requested number of cards as new deck object.
// the deck being delt from (the method reciever) is resliced.
func (d *deck) deal(n int) deck {
    if n > len(*d) {
        return nil
    }
    r := make(deck, n)
    copy(r, *d)
    *d = (*d)[n:]
    return r
}
