;define the whitespace as desired
#whitespace$ = " " + Chr($9) + Chr($A) + Chr($B) + Chr($C) + Chr($D) + Chr($1C) + Chr($1D) + Chr($1E) + Chr($1F)

Procedure.s myLTrim(source.s)
  Protected i, *ptrChar.Character, length = Len(source)
  *ptrChar = @source
  For i = 1 To length
    If Not FindString(#whitespace$, Chr(*ptrChar\c))
      ProcedureReturn Right(source, length + 1 - i)
    EndIf
    *ptrChar + SizeOf(Character)
  Next
EndProcedure

Procedure.s myRTrim(source.s)
  Protected i, *ptrChar.Character, length = Len(source)
  *ptrChar = @source + (length - 1) * SizeOf(Character)
  For i = length To 1 Step - 1
    If Not FindString(#whitespace$, Chr(*ptrChar\c))
      ProcedureReturn Left(source, i)
    EndIf
    *ptrChar - SizeOf(Character)
  Next
EndProcedure

Procedure.s myTrim(source.s)
  ProcedureReturn myRTrim(myLTrim(source))
EndProcedure

If OpenConsole()
  PrintN(#DQUOTE$ + myLTrim("  Top  ") + #DQUOTE$)
  PrintN(#DQUOTE$ + myRTrim("  Tail  ") + #DQUOTE$)
  PrintN(#DQUOTE$ +  myTrim("  Both  ") + #DQUOTE$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
