package main

import "fmt"

func bitwise(a, b int16) (and, or, xor, not, shl, shr, ras, rol, ror int16) {
    // the first four are easy
    and = a & b
    or = a | b
    xor = a ^ b
    not = ^a

    // for all shifts, the right operand (shift distance) must be unsigned.
    // use abs(b) for a non-negative value.
    if b < 0 {
        b = -b
    }
    ub := uint(b)

    shl = a << ub
    // for right shifts, if the left operand is unsigned, Go performs
    // a logical shift; if signed, an arithmetic shift.
    shr = int16(uint16(a) >> ub)
    ras = a >> ub

    // rotates
    rol = a << ub | int16(uint16(a) >> (16-ub))
    ror = int16(uint16(a) >> ub) | a << (16-ub)
    return
}

func main() {
    var a, b int16 = -460, 6
    and, or, xor, not, shl, shr, ras, rol, ror := bitwise(a, b)
    fmt.Printf("a:   %016b\n", uint16(a))
    fmt.Printf("b:   %016b\n", uint16(b))
    fmt.Printf("and: %016b\n", uint16(and))
    fmt.Printf("or:  %016b\n", uint16(or))
    fmt.Printf("xor: %016b\n", uint16(xor))
    fmt.Printf("not: %016b\n", uint16(not))
    fmt.Printf("shl: %016b\n", uint16(shl))
    fmt.Printf("shr: %016b\n", uint16(shr))
    fmt.Printf("ras: %016b\n", uint16(ras))
    fmt.Printf("rol: %016b\n", uint16(rol))
    fmt.Printf("ror: %016b\n", uint16(ror))
}
