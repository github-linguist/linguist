package main

import (
    "bytes"
    "encoding/binary"
    "fmt"
)

// declarations for word size of data
type word int32
const wordLen = 4
const highBit = -1 << 31

var data = []word{170, 45, 75, -90, -802, 24, 2, 66}

func main() {
    buf := bytes.NewBuffer(nil)
    ds := make([][]byte, len(data))
    for i, x := range data {
        binary.Write(buf, binary.LittleEndian, x^highBit)
        b := make([]byte, wordLen)
        buf.Read(b)
        ds[i] = b
    }
    bins := make([][][]byte, 256)
    for i := 0; i < wordLen; i++ {
        for _, b := range ds {
            bins[b[i]] = append(bins[b[i]], b)
        }
        j := 0
        for k, bs := range bins {
            copy(ds[j:], bs)
            j += len(bs)
            bins[k] = bs[:0]
        }
    }
    fmt.Println("original:", data)
    var w word
    for i, b := range ds {
        buf.Write(b)
        binary.Read(buf, binary.LittleEndian, &w)
        data[i] = w^highBit
    }
    fmt.Println("sorted:  ", data)
}
