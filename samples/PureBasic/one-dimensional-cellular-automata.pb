EnableExplicit
Dim cG.i(21)
Dim nG.i(21)
Define.i n, Gen

DataSection
  Data.i 0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0
EndDataSection
For n=1 To 20
  Read.i cG(n)
Next

OpenConsole()
Repeat
  Print("Generation "+Str(Gen)+": ")
  For n=1 To 20
    Print(Chr(95-cG(n)*60))
  Next
  Gen +1
  PrintN("")
  For n=1 To 20
    If (cG(n) And (cG(n-1) XOr cg(n+1))) Or (Not cG(n) And (cG(n-1) And cg(n+1)))
     nG(n)=1
   Else
     nG(n)=0
   EndIf
  Next
  Swap cG() , nG()
Until Gen > 9

PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
