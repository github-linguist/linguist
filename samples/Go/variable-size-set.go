package main

import (
    "fmt"
    "unsafe"
)

func main() {
    i := 5   // default type is int
    r := '5' // default type is rune (which is int32)
    f := 5.  // default type is float64
    c := 5i  // default type is complex128
    fmt.Println("i:", unsafe.Sizeof(i), "bytes")
    fmt.Println("r:", unsafe.Sizeof(r), "bytes")
    fmt.Println("f:", unsafe.Sizeof(f), "bytes")
    fmt.Println("c:", unsafe.Sizeof(c), "bytes")
    iMin := int8(5)
    rMin := byte('5')
    fMin := float32(5.)
    cMin := complex64(5i)
    fmt.Println("iMin:", unsafe.Sizeof(iMin), "bytes")
    fmt.Println("rMin:", unsafe.Sizeof(rMin), "bytes")
    fmt.Println("fMin:", unsafe.Sizeof(fMin), "bytes")
    fmt.Println("cMin:", unsafe.Sizeof(cMin), "bytes")
}
