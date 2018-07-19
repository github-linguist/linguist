Procedure.s RLDecode(toDecode.s)
  Protected.s repCount, output, currChar, tmp
  Protected *c.Character = @toDecode

  While *c\c <> #Null
    currChar = Chr(*c\c)
    Select *c\c
      Case '0' To '9'
        repCount + currChar
      Default
        If repCount
          tmp = Space(Val(repCount))
          ReplaceString(tmp, " ", currChar, #PB_String_InPlace)
          output + tmp
          repCount = ""
        Else
          output + currChar
        EndIf
    EndSelect
    *c + SizeOf(Character)
  Wend

  ProcedureReturn output
EndProcedure

Procedure.s RLEncode(toEncode.s)
  Protected.s currChar, prevChar, output
  Protected repCount
  Protected *c.Character = @toEncode

  prevChar = Chr(*c\c)
  repCount = 1

  *c + SizeOf(Character)
  While *c\c <> #Null
    currChar = Chr(*c\c)
    If currChar <> prevChar
      output + Str(repCount) + prevChar
      prevChar = currChar
      repCount = 1
    Else
      repCount + 1
    EndIf
    *c + SizeOf(Character)
  Wend

  output + Str(repCount)
  output + prevChar
  ProcedureReturn output
EndProcedure

If OpenConsole()
  Define initial.s, encoded.s, decoded.s

  Print("Type something: ")
  initial = Input()
  encoded = RLEncode(initial)
  decoded = RLDecode(encoded)
  PrintN(initial)
  PrintN(RLEncode(initial))
  PrintN(RLDecode(encoded))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
