Procedure Factor(n)
  Protected Result=1
  While n>0
    Result*n
    n-1
  Wend
  ProcedureReturn Result
EndProcedure

Macro C(n,k)
  (Factor(n)/(Factor(k)*factor(n-k)))
EndMacro

If OpenConsole()
  Print("Enter value n: "): n=Val(Input())
  Print("Enter value k: "): k=Val(Input())
  PrintN("C(n,k)= "+str(C(n,k)))

  Print("Press ENTER to quit"): Input()
  CloseConsole()
EndIf
