a = 1
a_next = b
b = 2
b_next = c
c = 3

traverse("a")
return

traverse(element)
{
  MsgBox % element . "= " . %element%
  name := element . "_next"
  while, %name%
  {
  element := %name%
  msgbox % %name% . "= " . %element%
  name := %name% . "_next"
  }
}
