; requires AutoHotkey_H or AutoHotkey.dll
msgbox % eval("3 + 4")
msgbox % eval("4 + 4")
return


eval(expression)
{
global script
script =
(
    expression(){
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
