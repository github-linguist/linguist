msgbox % first := evalWithX("x + 4", 5)
msgbox % second := evalWithX("x + 4", 6)
msgbox % second - first
return

evalWithX(expression, xvalue)
{
global script
script =
(
    expression(){
    x = %xvalue%  ; := would need quotes
    return %expression%
  }
)
renameFunction("expression", "")  ; remove any previous expressions
gosub load ; cannot use addScript inside a function yet
exp := "expression"
return %exp%()
}

load:
DllCall(A_AhkPath "\addScript","Str",script,"Uchar",0,"Cdecl UInt")
return

renameFunction(funcName, newname){
static
x%newname% := newname   ; store newname in a static variable so its memory is not freed
strput(newname, &x%newname%, strlen(newname) + 1)
if fnp := FindFunc(funcName)
  numput(&x%newname%, fnp+0, 0, "uint")
}
