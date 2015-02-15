Procedure.s stripChars(source.s,  charsToStrip.s)
  Protected i, *ptrChar.Character, length = Len(source), result.s
  *ptrChar = @source
  For i = 1 To length
    If Not FindString(charsToStrip, Chr(*ptrChar\c))
      result + Chr(*ptrChar\c)
    EndIf
    *ptrChar + SizeOf(Character)
  Next
  ProcedureReturn result
EndProcedure

If OpenConsole()
  PrintN(stripChars("She was a soul stripper. She took my heart!", "aei"))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
