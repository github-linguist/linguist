package main

import (
    "fmt"
    "math"
)

type testCase struct {
    hashCode string
    string
}

var testCases = []testCase{
    {"d41d8cd98f00b204e9800998ecf8427e", ""},
    {"0cc175b9c0f1b6a831c399e269772661", "a"},
    {"900150983cd24fb0d6963f7d28e17f72", "abc"},
    {"f96b697d7cb7938d525a2f31aaf161d0", "message digest"},
    {"c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz"},
    {"d174ab98d277d9f5a5611c2c9f419d9f",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"},
    {"57edf4a22be3c955ac49da2e2107b67a", "12345678901234567890" +
        "123456789012345678901234567890123456789012345678901234567890"},
}

func main() {
    for _, tc := range testCases {
        fmt.Printf("%s\n%x\n\n", tc.hashCode, md5(tc.string))
    }
}

var shift = [...]uint{7, 12, 17, 22, 5, 9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21}
var table [64]uint32

func init() {
    for i := range table {
        table[i] = uint32((1 << 32) * math.Abs(math.Sin(float64(i + 1))))
    }
}

func md5(s string) (r [16]byte) {
    numBlocks := (len(s) + 72) >> 6
    padded := make([]byte, numBlocks<<6)
    copy(padded, s)
    padded[len(s)] = 0x80
    for i, messageLenBits := len(padded)-8, len(s)<<3; i < len(padded); i++ {
        padded[i] = byte(messageLenBits)
        messageLenBits >>= 8
    }

    var a, b, c, d uint32 = 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476
    var buffer [16]uint32
    for i := 0; i < numBlocks; i++ {
        index := i << 6
        for j := 0; j < 64; j, index = j+1, index+1 {
            buffer[j>>2] = (uint32(padded[index]) << 24) | (buffer[j>>2] >> 8)
        }
        a1, b1, c1, d1 := a, b, c, d
        for j := 0; j < 64; j++ {
            var f uint32
            bufferIndex := j
            round := j >> 4
            switch round {
            case 0:
                f = (b1 & c1) | (^b1 & d1)
            case 1:
                f = (b1 & d1) | (c1 & ^d1)
                bufferIndex = (bufferIndex*5 + 1) & 0x0F
            case 2:
                f = b1 ^ c1 ^ d1
                bufferIndex = (bufferIndex*3 + 5) & 0x0F
            case 3:
                f = c1 ^ (b1 | ^d1)
                bufferIndex = (bufferIndex * 7) & 0x0F
            }
            sa := shift[(round<<2)|(j&3)]
            a1 += f + buffer[bufferIndex] + table[j]
            a1, d1, c1, b1 = d1, c1, b1, a1<<sa|a1>>(32-sa)+b1
        }
        a, b, c, d = a+a1, b+b1, c+c1, d+d1
    }

    for i, n := range []uint32{a, b, c, d} {
        for j := 0; j < 4; j++ {
            r[i*4+j] = byte(n)
            n >>= 8
        }
    }
    return
}
