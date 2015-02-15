package main

import "fmt"

type vkey string

func newVigenère(key string) (vkey, bool) {
    v := vkey(upperOnly(key))
    return v, len(v) > 0 // key length 0 invalid
}

func (k vkey) encipher(pt string) string {
    ct := upperOnly(pt)
    for i, c := range ct {
        ct[i] = 'A' + (c-'A'+k[i%len(k)]-'A')%26
    }
    return string(ct)
}

func (k vkey) decipher(ct string) (string, bool) {
    pt := make([]byte, len(ct))
    for i := range pt {
        c := ct[i]
        if c < 'A' || c > 'Z' {
            return "", false // invalid ciphertext
        }
        pt[i] = 'A' + (c-k[i%len(k)]+26)%26
    }
    return string(pt), true
}

// upperOnly extracts letters A-Z, a-z from a string and
// returns them all upper case in a byte slice.
// Useful for vkey constructor and encipher function.
func upperOnly(s string) []byte {
    u := make([]byte, 0, len(s))
    for i := 0; i < len(s); i++ {
        c := s[i]
        if c >= 'A' && c <= 'Z' {
            u = append(u, c)
        } else if c >= 'a' && c <= 'z' {
            u = append(u, c-32)
        }
    }
    return u
}

const testKey = "Vigenère Cipher"
const testPT = `Beware the Jabberwock, my son!
    The jaws that bite, the claws that catch!`

func main() {
    fmt.Println("Supplied key: ", testKey)
    v, ok := newVigenère(testKey)
    if !ok {
        fmt.Println("Invalid key")
        return
    }
    fmt.Println("Effective key:", v)
    fmt.Println("Plain text:", testPT)
    ct := v.encipher(testPT)
    fmt.Println("Enciphered:", ct)
    dt, ok := v.decipher(ct)
    if !ok {
        fmt.Println("Invalid ciphertext")
        return
    }
    fmt.Println("Deciphered:", dt)
}
