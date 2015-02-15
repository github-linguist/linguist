package main

import (
    "crypto/md5"
    "fmt"
)

func main() {
    for _, p := range [][2]string{
        // RFC 1321 test cases
        {"d41d8cd98f00b204e9800998ecf8427e", ""},
        {"0cc175b9c0f1b6a831c399e269772661", "a"},
        {"900150983cd24fb0d6963f7d28e17f72", "abc"},
        {"f96b697d7cb7938d525a2f31aaf161d0", "message digest"},
        {"c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz"},
        {"d174ab98d277d9f5a5611c2c9f419d9f",
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"},
        {"57edf4a22be3c955ac49da2e2107b67a", "12345678901234567890" +
            "123456789012345678901234567890123456789012345678901234567890"},
        // test case popular with other RC solutions
        {"e38ca1d920c4b8b8d3946b2c72f01680",
            "The quick brown fox jumped over the lazy dog's back"},
    } {
        validate(p[0], p[1])
    }
}

var h = md5.New()

func validate(check, s string) {
    h.Reset()
    h.Write([]byte(s))
    sum := fmt.Sprintf("%x", h.Sum(nil))
    if sum != check {
        fmt.Println("MD5 fail")
        fmt.Println("  for string,", s)
        fmt.Println("  expected:  ", check)
        fmt.Println("  got:       ", sum)
    }
}
