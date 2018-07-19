If OpenConsole()
  Define x, y

  Print("Input X-Size: ")
  x = Val(Input())

  Print("Input Y-Size: ")
  y = Val(Input())

  Dim a(x,y)   ; Should really check if x & y are larger then 1, but that would be less fun....

  a(1,1)=Random(1000)
  PrintN("a(1,1)= " + Str(a(1,1)) )

  PrintN("Press ENTER to exit"):Input()
  End          ; Close down and let PureBasic delete the Console and all variables.
EndIf
