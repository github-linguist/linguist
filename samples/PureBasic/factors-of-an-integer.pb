Procedure PrintFactors(n)
  Protected i, lim=Round(sqr(n),#PB_Round_Up)
  NewList F.i()
  For i=1 To lim
    If n%i=0
      AddElement(F()): F()=i
      AddElement(F()): F()=n/i
    EndIf
  Next
  ;- Present the result
  SortList(F(),#PB_Sort_Ascending)
  ForEach F()
    Print(str(F())+" ")
  Next
EndProcedure

If OpenConsole()
  Print("Enter integer to factorize: ")
  PrintFactors(Val(Input()))
  Print(#CRLF$+#CRLF$+"Press ENTER to quit."): Input()
EndIf
