package main

import (
    "fmt"
    "strings"
)

// R1: representation is a slice of int8 digits of -1, 0, or 1.
// digit at index 0 is least significant.  zero value of type is
// representation of the number 0.
type bt []int8

// R2: string conversion:

// btString is a constructor.  valid input is a string of any length
// consisting of only '+', '-', and '0' characters.
// leading zeros are allowed but are trimmed and not represented.
// false return means input was invalid.
func btString(s string) (*bt, bool) {
    s = strings.TrimLeft(s, "0")
    b := make(bt, len(s))
    for i, last := 0, len(s)-1; i < len(s); i++ {
        switch s[i] {
        case '-':
            b[last-i] = -1
        case '0':
            b[last-i] = 0
        case '+':
            b[last-i] = 1
        default:
            return nil, false
        }
    }
    return &b, true
}

// String method converts the other direction, returning a string of
// '+', '-', and '0' characters representing the number.
func (b bt) String() string {
    if len(b) == 0 {
        return "0"
    }
    last := len(b) - 1
    r := make([]byte, len(b))
    for i, d := range b {
        r[last-i] = "-0+"[d+1]
    }
    return string(r)
}

// R3: integer conversion
// int chosen as "native integer"

// btInt is a constructor like btString.
func btInt(i int) *bt {
    if i == 0 {
        return new(bt)
    }
    var b bt
    var btDigit func(int)
    btDigit = func(digit int) {
        m := int8(i % 3)
        i /= 3
        switch m {
        case 2:
            m = -1
            i++
        case -2:
            m = 1
            i--
        }
        if i == 0 {
            b = make(bt, digit+1)
        } else {
            btDigit(digit + 1)
        }
        b[digit] = m
    }
    btDigit(0)
    return &b
}

// Int method converts the other way, returning the value as an int type.
// !ok means overflow occurred during conversion, not necessarily that the
// value is not representable as an int.  (Of course there are other ways
// of doing it but this was chosen as "reasonable.")
func (b bt) Int() (r int, ok bool) {
    pt := 1
    for _, d := range b {
        dp := int(d) * pt
        neg := r < 0
        r += dp
        if neg {
            if r > dp {
                return 0, false
            }
        } else {
            if r < dp {
                return 0, false
            }
        }
        pt *= 3
    }
    return r, true
}

// R4: negation, addition, and multiplication

func (z *bt) Neg(b *bt) *bt {
    if z != b {
        if cap(*z) < len(*b) {
            *z = make(bt, len(*b))
        } else {
            *z = (*z)[:len(*b)]
        }
    }
    for i, d := range *b {
        (*z)[i] = -d
    }
    return z
}

func (z *bt) Add(a, b *bt) *bt {
    if len(*a) < len(*b) {
        a, b = b, a
    }
    r := *z
    r = r[:cap(r)]
    var carry int8
    for i, da := range *a {
        if i == len(r) {
            n := make(bt, len(*a)+4)
            copy(n, r)
            r = n
        }
        sum := da + carry
        if i < len(*b) {
            sum += (*b)[i]
        }
        carry = sum / 3
        sum %= 3
        switch {
        case sum > 1:
            sum -= 3
            carry++
        case sum < -1:
            sum += 3
            carry--
        }
        r[i] = sum
    }
    last := len(*a)
    if carry != 0 {
        if len(r) == last {
            n := make(bt, last+4)
            copy(n, r)
            r = n
        }
        r[last] = carry
        *z = r[:last+1]
        return z
    }
    for {
        if last == 0 {
            *z = nil
            break
        }
        last--
        if r[last] != 0 {
            *z = r[:last+1]
            break
        }
    }
    return z
}

func (z *bt) Mul(a, b *bt) *bt {
    if len(*a) < len(*b) {
        a, b = b, a
    }
    var na bt
    for _, d := range *b {
        if d == -1 {
            na.Neg(a)
            break
        }
    }
    r := make(bt, len(*a)+len(*b))
    for i := len(*b) - 1; i >= 0; i-- {
        switch (*b)[i] {
        case 1:
            p := r[i:]
            p.Add(&p, a)
        case -1:
            p := r[i:]
            p.Add(&p, &na)
        }
    }
    i := len(r)
    for i > 0 && r[i-1] == 0 {
        i--
    }
    *z = r[:i]
    return z
}

func main() {
    a, _ := btString("+-0++0+")
    b := btInt(-436)
    c, _ := btString("+-++-")
    show("a:", a)
    show("b:", b)
    show("c:", c)
    show("a(b-c):", a.Mul(a, b.Add(b, c.Neg(c))))
}

func show(label string, b *bt) {
    fmt.Printf("%7s %12v ", label, b)
    if i, ok := b.Int(); ok {
        fmt.Printf("%7d\n", i)
    } else {
        fmt.Println("int overflow")
    }
}
