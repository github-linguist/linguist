Procedure.i gray_encode(n)
  ProcedureReturn n ! (n >> 1)
EndProcedure

Procedure.i gray_decode(g)
  Protected bit = 1 << (8 * SizeOf(Integer) - 2)
  Protected b = g & bit, p = b >> 1

  While bit > 1
    bit >> 1
    b | (p ! (g & bit))
    p = (b & bit) >> 1
  Wend
  ProcedureReturn b
EndProcedure

If OpenConsole()
  PrintN("Number Binary Gray    Decoded")
  Define i, n
  For i = 0 To 31
    g = gray_encode(i)
    Print(RSet(Str(i), 2, "0") + Space(5))
    Print(RSet(Bin(g, #PB_Byte), 5, "0") + Space(2))
    n = gray_decode(g)
    Print(RSet(Bin(n, #PB_Byte), 5, "0") + Space(3))
    PrintN(RSet(Str(n), 2, "0"))
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
