i = 1
j = 1
x := a(i) and b(j)
y := a(i) or b(j)

a(p)
{
 MsgBox, a() was called with the parameter "%p%".
 Return, p
}

b(p)
{
 MsgBox, b() was called with the parameter "%p%".
 Return, p
}
