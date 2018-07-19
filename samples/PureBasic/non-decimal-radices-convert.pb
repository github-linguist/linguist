Global alphanum$ = "0123456789abcdefghijklmnopqrstuvwxyz" ;36 digits
#maxIntegerBitSize = SizeOf(Integer) * 8

Procedure toDecimal(base, s.s)
  Protected length, i, toDecimal

  length = Len(s)
  If length: toDecimal = FindString(alphanum$, Left(s, 1), 1) - 1: EndIf

  For i = 2 To length
    toDecimal * base + FindString(alphanum$, Mid(s, i, 1), 1) - 1
  Next
  ProcedureReturn toDecimal
EndProcedure

Procedure.s toBase(base, number)
  Protected i, rem, toBase.s{#maxIntegerBitSize} = Space(#maxIntegerBitSize)

  For i = #maxIntegerBitSize To 1 Step -1
    rem = number % base
    PokeC(@toBase + i - 1, PeekC(@alphanum$ + rem))
    If number < base: Break: EndIf
    number / base
  Next
  ProcedureReturn LTrim(toBase)
EndProcedure

If OpenConsole()
  PrintN( Str(toDecimal(16, "1a")) )

  PrintN( toBase(16, 26) )

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
