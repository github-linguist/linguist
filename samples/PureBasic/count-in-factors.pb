Procedure Factorize(Number, List Factors())
  Protected I = 3, Max
  ClearList(Factors())
  While Number % 2 = 0
    AddElement(Factors())
    Factors() = 2
    Number / 2
  Wend
  Max = Number
  While I <= Max And Number > 1
    While Number % I = 0
      AddElement(Factors())
      Factors() = I
      Number / I
    Wend
    I + 2
  Wend
EndProcedure

If OpenConsole()
  NewList n()
  For a=1 To 20
    text$=RSet(Str(a),2)+"= "
    Factorize(a,n())
    If ListSize(n())
      ResetList(n())
      While NextElement(n())
        text$ + Str(n())
        If ListSize(n())-ListIndex(n())>1
          text$ + "*"
        EndIf
      Wend
    Else
      text$+Str(a) ; To handle the '1', which is not really a prime...
    EndIf
    PrintN(text$)
  Next a
EndIf
