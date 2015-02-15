Procedure.s stripControlCodes(source.s)
  Protected i, *ptrChar.Character, length = Len(source), result.s
  *ptrChar = @source
  For i = 1 To length
    If *ptrChar\c > 31
      result + Chr(*ptrChar\c)
    EndIf
    *ptrChar + SizeOf(Character)
  Next
  ProcedureReturn result
EndProcedure

Procedure.s stripControlExtCodes(source.s)
  Protected i, *ptrChar.Character, length = Len(source), result.s
  *ptrChar = @source
  For i = 1 To length
    If *ptrChar\c > 31 And *ptrChar\c < 128
      result + Chr(*ptrChar\c)
    EndIf
    *ptrChar + SizeOf(Character)
  Next
  ProcedureReturn result
EndProcedure

If OpenConsole()
  ;create sample string
  Define i, s.s
  For i = 1 To 80
    s + Chr(Random(254) + 1) ;include character values from 1 to 255
  Next

  PrintN(stripControlCodes(s))    ;string without control codes
  PrintN("---------")
  PrintN(stripControlExtCodes(s)) ;string without control codes or extended chars

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
