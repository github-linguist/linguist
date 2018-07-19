NewMap threads()

Procedure Foo(n)
  Delay(n)
  PrintN(Str(n))
EndProcedure

If OpenConsole()
  For i=1 To CountProgramParameters()
    threads(Str(i)) = CreateThread(@Foo(), Val(ProgramParameter()))
  Next

  ForEach threads()
    WaitThread(threads())
  Next
  Print("Press ENTER to exit"): Input()
EndIf
