package pcre

/*
#cgo LDFLAGS: -lpcre
#include <pcre.h>
*/
import "C"

import (
	"fmt"
	"strings"
	"unsafe"
)

func RegexPP(re string) string {
	if len(re) > 32 {
		re = fmt.Sprintf("\"`%s`...\"", re[:32])
	} else {
		re = fmt.Sprintf("\"`%s`\"", re)
	}
	return strings.Replace(re, "\n", "", -1)
}

type CompileError struct {
	Pattern string
	Message string
	Offset  int
}

func (e *CompileError) Error() string {
	return fmt.Sprintf("regex %s: %s (at offset %d)",
		RegexPP(e.Pattern), e.Message, e.Offset)
}

const DefaultFlags = int(C.PCRE_DUPNAMES | C.PCRE_UTF8 | C.PCRE_NEWLINE_ANYCRLF)

func CheckRegexp(pattern string, flags int) error {
	pattern1 := C.CString(pattern)
	defer C.free(unsafe.Pointer(pattern1))

	var errptr *C.char
	var erroffset C.int
	ptr := C.pcre_compile(pattern1, C.int(flags), &errptr, &erroffset, nil)
	if ptr == nil {
		return &CompileError{
			Pattern: pattern,
			Message: C.GoString(errptr),
			Offset:  int(erroffset),
		}
	}
	C.free(unsafe.Pointer(ptr))
	return nil
}
