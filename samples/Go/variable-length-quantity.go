package main

import (
    "fmt"
    "encoding/binary"
)

func main() {
    buf := make([]byte, binary.MaxVarintLen64)
    for _, x := range []int64{0x200000, 0x1fffff} {
        v := buf[:binary.PutVarint(buf, x)]
        fmt.Printf("%d encodes into %d bytes: %x\n", x, len(v), v)
        x, _ = binary.Varint(v)
        fmt.Println(x, "decoded")
    }
}
