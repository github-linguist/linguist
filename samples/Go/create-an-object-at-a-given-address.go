package main

import(
	"fmt"
	"unsafe"
	"reflect"
)

func pointer() {
	fmt.Printf("Pointer:\n")

	// Create a *int and store the address of 'i' in it. To create a pointer to
	// an arbitrary memory location, use something like the following:
	//    p := (*int)(unsafe.Pointer(uintptr(0x100)))
	// And replace '0x100' with the desired address.
	var i int
	p := &i

	fmt.Printf("Before:\n\t%v: %v, %v\n", p, *p, i)

	*p = 3

	fmt.Printf("After:\n\t%v: %v, %v\n", p, *p, i)
}

func slice() {
	fmt.Printf("Slice:\n")

	var a [10]byte

	// reflect.SliceHeader is a runtime representation of the internal workings
	// of a slice. To make it point to a specific address, use something like
	// the following:
	//    h.Data = uintptr(0x100)
	// And replace '0x100' with the desired address.
	var h reflect.SliceHeader
	h.Data = uintptr(unsafe.Pointer(&a)) // The address of the first element of the underlying array.
	h.Len = len(a)
	h.Cap = len(a)

	// Create an actual slice from the SliceHeader.
	s := *(*[]byte)(unsafe.Pointer(&h))

	fmt.Printf("Before:\n\ts: %v\n\ta: %v\n", s, a)

	// Copy a string into the slice. This fills the underlying array, which in
	// this case has been manually set to 'a'.
	copy(s, "A string.")

	fmt.Printf("After:\n\ts: %v\n\ta: %v\n", s, a)
}

func main() {
	pointer()
	fmt.Println()

	slice()
}
