package main

import (
    "bytes"
    "crypto/sha256"
    "errors"
    "os"
)

// With at least one other bitcoin RC task, this source is styled more like
// a package to show how functions of the two tasks might be combined into
// a single package.  It turns out there's not really that much shared code,
// just the A25 type and doubleSHA256 method, but it's enough to suggest how
// the code might be organized.  Types, methods, and functions are capitalized
// where they might be exported from a package.

// A25 is a type for a 25 byte (not base58 encoded) bitcoin address.
type A25 [25]byte

func (a *A25) Version() byte {
    return a[0]
}

func (a *A25) EmbeddedChecksum() (c [4]byte) {
    copy(c[:], a[21:])
    return
}

// DoubleSHA256 computes a double sha256 hash of the first 21 bytes of the
// address.  This is the one function shared with the other bitcoin RC task.
// Returned is the full 32 byte sha256 hash.  (The bitcoin checksum will be
// the first four bytes of the slice.)
func (a *A25) doubleSHA256() []byte {
    h := sha256.New()
    h.Write(a[:21])
    d := h.Sum([]byte{})
    h = sha256.New()
    h.Write(d)
    return h.Sum(d[:0])
}

// ComputeChecksum returns a four byte checksum computed from the first 21
// bytes of the address.  The embedded checksum is not updated.
func (a *A25) ComputeChecksum() (c [4]byte) {
    copy(c[:], a.doubleSHA256())
    return
}/* {{header|Go}} */

// Tmpl and Set58 are adapted from the C solution.
// Go has big integers but this techinique seems better.
var tmpl = []byte("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

// Set58 takes a base58 encoded address and decodes it into the receiver.
// Errors are returned if the argument is not valid base58 or if the decoded
// value does not fit in the 25 byte address.  The address is not otherwise
// checked for validity.
func (a *A25) Set58(s []byte) error {
    for _, s1 := range s {
        c := bytes.IndexByte(tmpl, s1)
        if c < 0 {
            return errors.New("bad char")
        }
        for j := 24; j >= 0; j-- {
            c += 58 * int(a[j])
            a[j] = byte(c % 256)
            c /= 256
        }
        if c > 0 {
            return errors.New("too long")
        }
    }
    return nil
}

// ValidA58 validates a base58 encoded bitcoin address.  An address is valid
// if it can be decoded into a 25 byte address, the version number is 0,
// and the checksum validates.  Return value ok will be true for valid
// addresses.  If ok is false, the address is invalid and the error value
// may indicate why.
func ValidA58(a58 []byte) (ok bool, err error) {
    var a A25
    if err := a.Set58(a58); err != nil {
        return false, err
    }
    if a.Version() != 0 {
        return false, errors.New("not version 0")
    }
    return a.EmbeddedChecksum() == a.ComputeChecksum(), nil
}

// Program returns exit code 0 with valid address and produces no output.
// Otherwise exit code is 1 and a message is written to stderr.
func main() {
    if len(os.Args) != 2 {
        errorExit("Usage: valid <base58 address>")
    }
    switch ok, err := ValidA58([]byte(os.Args[1])); {
    case ok:
    case err == nil:
        errorExit("Invalid")
    default:
        errorExit(err.Error())
    }
}

func errorExit(m string) {
    os.Stderr.WriteString(m + "\n")
    os.Exit(1)
}
