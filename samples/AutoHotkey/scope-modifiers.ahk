singleton = "global variable"

assume_global()
{
  Global  ; assume all variables declared in this function are global in scope
  Static callcount := 0   ; except this one declared static, initialized once only
  MsgBox % singleton  ; usefull to initialize a bunch of singletons
  callcount++
}

assume_global2()
{
  Local var1  ; assume global except for var1  (similar to global scope declaration)
  MsgBox % singleton
}

object(member, value = 0, null = 0)
{
  Static  ; assume all variables in this function to be static
  If value    ; can be used to simulate objects
	_%member% := value
  Else If null
	_%member% := ""
  Return (_%member%)
}
