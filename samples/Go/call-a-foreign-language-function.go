package main

// #include <string.h>
// #include <stdlib.h>
import "C"
import (
    "fmt"
    "unsafe"
)

func main() {
    // a go string
    go1 := "hello C"
    // allocate in C and convert from Go representation to C representation
    c1 := C.CString(go1)
    // go string can now be garbage collected
    go1 = ""
    // strdup, per task. this calls the function in the C library.
    c2 := C.strdup(c1)
    // free the source C string.  again, this is free() in the C library.
    C.free(unsafe.Pointer(c1))
    // create a new Go string from the C copy
    go2 := C.GoString(c2)
    // free the C copy
    C.free(unsafe.Pointer(c2))
    // demonstrate we have string contents intact
    fmt.Println(go2)
}
