; Call a function without arguments:
f()

; Call a function with a fixed number of arguments:
f("string", var, 15.5)

; Call a function with optional arguments:
f("string", var, 15.5)

; Call a function with a variable number of arguments:
f("string", var, 15.5)

; Call a function with named arguments:
    ; AutoHotkey does not have named arguments. However, in v1.1+,
    ; we can pass an object to the function:
f({named: "string", otherName: var, thirdName: 15.5})

; Use a function in statement context:
f(1), f(2) ; What is statement context?

; No first-class functions in AHK

; Obtaining the return value of a function:
varThatGetsReturnValue := f(1, "a")

; Cannot distinguish built-in functions

; Subroutines are called with GoSub; functions are called as above.
; Subroutines cannot be passed variables

; Stating whether arguments are passed by value or by reference:
; [v1.1.01+]: The IsByRef() function can be used to determine
;     whether the caller supplied a variable for a given ByRef parameter.
; A variable cannot be passed by value to a byRef parameter. Instead, do this:
f(tmp := varIdoNotWantChanged)
; the function f will receive the value of varIdoNotWantChanged, but any
; modifications will be made to the variable tmp.

; Partial application is impossible.
