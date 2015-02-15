x = hello       ; assign verbatim as a string
z := 3 + 4      ; assign an expression
if !y       ; uninitialized variables are assumed to be 0 or "" (blank string)
Msgbox %x%  ; variable dereferencing is done by surrounding '%' signs
fx()
{
local x   ; variable default scope in a function is local anyways
global y  ;
static z=4  ; initialized once, then value is remembered between function calls
}
