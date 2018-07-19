msgbox % stack("push", 4)
msgbox % stack("push", 5)
msgbox % stack("peek")
msgbox % stack("pop")
msgbox % stack("peek")
msgbox % stack("empty")
msgbox % stack("pop")
msgbox % stack("empty")
return

stack(command, value = 0)
{
  static
if !pointer
pointer = 10000
  if (command = "push")
  {
  _p%pointer% := value
  pointer -= 1
  return value
  }
  if (command = "pop")
  {
    pointer += 1
    return _p%pointer%
  }
  if (command = "peek")
{
next := pointer + 1
return _p%next%
}
  if (command = "empty")
  {
   if (pointer == 10000)
    return "empty"
else
return 0
  }
}
