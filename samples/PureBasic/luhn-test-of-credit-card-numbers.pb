DataSection
  Sample:
  Data.s "49927398716"
  Data.s "49927398717"
  Data.s "1234567812345678"
  Data.s "1234567812345670"
  Data.s ""
EndDataSection

Procedure isValid(cardNumber.s)
  Protected i, length, s1, s2, s2a

  cardNumber = ReverseString(cardNumber)
  length = Len(cardNumber)
  For i = 1 To length Step 2
    s1 + Val(Mid(cardNumber, i, 1))
  Next

  For i = 2 To length Step 2
    s2a = Val(Mid(cardNumber, i, 1)) * 2
    If s2a < 10
      s2 + s2a
    Else
      s2 + 1 + Val(Right(Str(s2a), 1))
    EndIf
  Next

  If Right(Str(s1 + s2), 1) = "0"
    ProcedureReturn #True
  Else
    ProcedureReturn #False
  EndIf
EndProcedure


If OpenConsole()
  Define cardNumber.s

  Restore Sample
  Repeat
    Read.s cardNumber
    If cardNumber <> ""
      Print(cardNumber + " is ")
      If isValid(cardNumber)
        PrintN("valid")
      Else
        PrintN("not valid")
      EndIf
    EndIf
  Until cardNumber = ""

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
