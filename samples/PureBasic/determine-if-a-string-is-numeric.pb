Procedure IsNumeric(InString.s, DecimalCharacter.c = '.')
  #NotNumeric = #False
  #IsNumeric = #True

  InString = Trim(InString)
  Protected IsDecimal, CaughtDecimal, CaughtE
  Protected IsSignPresent, IsSignAllowed = #True, CountNumeric
  Protected *CurrentChar.Character = @InString

  While *CurrentChar\c
    Select *CurrentChar\c
      Case '0' To '9'
        CountNumeric + 1
        IsSignAllowed = #False
      Case DecimalCharacter
        If CaughtDecimal Or CaughtE Or CountNumeric = 0
          ProcedureReturn #NotNumeric
        EndIf

        CountNumeric = 0
        CaughtDecimal = #True
        IsDecimal = #True
      Case  '-', '+'
        If IsSignPresent Or Not IsSignAllowed: ProcedureReturn #NotNumeric: EndIf
        IsSignPresent = #True
      Case 'E', 'e'
        If CaughtE Or CountNumeric = 0
          ProcedureReturn #NotNumeric
        EndIf

        CaughtE = #True
        CountNumeric = 0
        CaughtDecimal = #False
        IsSignPresent = #False
        IsSignAllowed = #True
      Default
        ProcedureReturn #NotNumeric
    EndSelect
    *CurrentChar + SizeOf(Character)
  Wend

  If CountNumeric = 0: ProcedureReturn #NotNumeric: EndIf
  ProcedureReturn #IsNumeric
EndProcedure

If OpenConsole()
  PrintN("'+3183.31151E+321' = " + Str(IsNumeric("+3183.31151E+321")))
  PrintN("'-123456789' = " + Str(IsNumeric("-123456789")))
  PrintN("'123.45.6789+' = " + Str(IsNumeric("123.45.6789+")))
  PrintN("'-e' = " + Str(IsNumeric("-e")))
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
