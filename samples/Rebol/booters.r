REBOL [
	System: "REBOL [R3] Language Interpreter and Run-time Environment"
	Title: "Special boot native function specs"
	Rights: {
		Copyright 2012 REBOL Technologies
		REBOL is a trademark of REBOL Technologies
	}
	License: {
		Licensed under the Apache License, Version 2.0.
		See: http://www.apache.org/licenses/LICENSE-2.0
	}
	Purpose: {
		These are used to define natives and actions.
		Bind attributes for this block are: BIND_SET and SHALLOW
	}
]

; Special block used as spec to the datatype test functions (e.g. time?):
["Returns TRUE if it is this type." value [any-type!] 0]

; The native function must be defined first. This is a
; special boot function created manually within the C code.
native: native [
	{Creates native function (for internal usage only).}
	spec ; [block!] -- no check required, we know it is correct
]

action: native [
	{Creates datatype action (for internal usage only).}
	spec ; [block!] -- no check required, we know it is correct
]
