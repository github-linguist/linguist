package main

import (
    "bytes"
    "fmt"
)

// Strings in Go allow arbitrary bytes.  They are implemented basically as
// immutable byte slices and syntactic sugar.  This program shows functions
// required by the task on byte slices, thus it mostly highlights what
// happens behind the syntactic sugar.  The program does not attempt to
// reproduce the immutability property of strings, as that does not seem
// to be the intent of the task.

func main() {
    // Task point: String creation and destruction.
    // Strings are most often constructed from literals as in s := "binary"
    // With byte slices,
    b := []byte{'b', 'i', 'n', 'a', 'r', 'y'}
    fmt.Println(b) // output shows numeric form of bytes.
    // Go is garbage collected.  There are no destruction operations.

    // Task point: String assignment.
    // t = s assigns strings.  Since strings are immutable, it is irrelevant
    // whether the string is copied or not.
    // With byte slices, the same works,
    var c []byte
    c = b
    fmt.Println(c)

    // Task point: String comparison.
    // operators <, <=, ==, >=, and > work directly on strings comparing them
    // by lexicographic order.
    // With byte slices, there are standard library functions, bytes.Equal
    // and bytes.Compare.
    fmt.Println(bytes.Equal(b, c)) // prints true

    // Task point: String cloning and copying.
    // The immutable property of Go strings makes cloning and copying
    // meaningless for strings.
    // With byte slices though, it is relevant.  The assignment c = b shown
    // above does a reference copy, leaving both c and b based on the same
    // underlying data.  To clone or copy the underlying data,
    d := make([]byte, len(b)) // allocate new space
    copy(d, b)                // copy the data
    // The data can be manipulated independently now:
    d[1] = 'a'
    d[4] = 'n'
    fmt.Println(string(b)) // convert to string for readable output
    fmt.Println(string(d))

    // Task point: Check if a string is empty.
    // Most typical for strings is s == "", but len(s) == 0 works too.
    // For byte slices, "" does not work, len(b) == 0 is correct.
    fmt.Println(len(b) == 0)

    // Task point: Append a byte to a string.
    // The language does not provide a way to do this directly with strings.
    // Instead, the byte must be converted to a one-byte string first, as in,
    // s += string('z')
    // For byte slices, the language provides the append function,
    z := append(b, 'z')
    fmt.Printf("%s\n", z) // another way to get readable output

    // Task point: Extract a substring from a string.
    // Slicing syntax is the for both strings and slices.
    sub := b[1:3]
    fmt.Println(string(sub))

    // Task point: Replace every occurrence of a byte (or a string)
    // in a string with another string.
    // Go supports this with similar library functions for strings and
    // byte slices.  Strings:  t = strings.Replace(s, "n", "m", -1).
    // The byte slice equivalent returns a modified copy, leaving the
    // original byte slice untouched,
    f := bytes.Replace(d, []byte{'n'}, []byte{'m'}, -1)
    fmt.Printf("%s -> %s\n", d, f)

    // Task point: Join strings.
    // Using slicing syntax again, with strings,
    // rem := s[:1] + s[3:] leaves rem == "bary".
    // Only the concatenation of the parts is different with byte slices,
    rem := append(append([]byte{}, b[:1]...), b[3:]...)
    fmt.Println(string(rem))
}
