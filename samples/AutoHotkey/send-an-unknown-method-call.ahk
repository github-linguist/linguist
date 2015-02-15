obj := {mA: Func("mA"), mB: Func("mB"), mC: Func("mC")}
InputBox, methodToCall, , Which method should I call?
obj[methodToCall].()

mA(){
 MsgBox Method A
}
mB(){
 MsgBox Method B
}
mC(){
 MsgBox Method C
}
