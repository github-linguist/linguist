b1 := borg()
b2 := borg()
msgbox % "b1 is b2? " . (b1 == b2)
b1.datum := 3
msgbox % "b1.datum := 3`n...`nb1 datum: " b1.datum "`nb2 datum: " b2.datum ; is 3 also
msgbox % "b1.datum is b2.datum ? " (b1.datum == b2.datum)
return


borg(){
   static borg
   If !borg
      borg := Object("__Set", "Borg_Set"
                   , "__Get", "Borg_Get")
   return object(1, borg, "base", borg)
}


Borg_Get(brg, name)
{
  Return brg[1, name]
}

Borg_Set(brg, name, val)
{
  brg[1, name] := val
  Return val
}
