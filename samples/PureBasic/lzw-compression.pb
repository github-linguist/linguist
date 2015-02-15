Procedure compress(uncompressed.s, List result.u())
  ;Compress a string to a list of output symbols

  ;Build the dictionary.
  Protected  dict_size = 255, i
  newmap dict.u()
  For i = 0 To 254
    dict(Chr(i + 1)) = i
  Next

  Protected w.s, wc.s, *c.Character = @uncompressed
  w = ""
  LastElement(result())
  While *c\c <> #Null
    wc = w + Chr(*c\c)
    If FindMapElement(dict(), wc)
      w = wc
    Else
      AddElement(result())
      result() = dict(w)
      ;Add wc to the dictionary
      dict(wc) = dict_size
      dict_size + 1 ;no check is performed for overfilling the dictionary.
      w = Chr(*c\c)
    EndIf
    *c + 1
  Wend

  ;Output the code for w
  If w
    AddElement(result())
    result() = dict(w)
  EndIf
EndProcedure

Procedure.s decompress(List compressed.u())
  ;Decompress a list of encoded values to a string
  If ListSize(compressed()) = 0: ProcedureReturn "": EndIf

  ;Build the dictionary.
  Protected  dict_size = 255, i

  Dim dict.s(255)
  For i = 1 To 255
    dict(i - 1) = Chr(i)
  Next

  Protected w.s, entry.s, result.s
  FirstElement(compressed())
  w = dict(compressed())
  result = w

  i = 0
  While NextElement(compressed())
    i + 1
    If compressed() < dict_size
    entry = dict(compressed())
    ElseIf i = dict_size
      entry = w + Left(w, 1)
    Else
      MessageRequester("Error","Bad compression at [" + Str(i) + "]")
      ProcedureReturn result;abort
    EndIf
    result + entry
    ;Add w + Left(entry, 1) to the dictionary
    If ArraySize(dict()) <= dict_size
      Redim dict(dict_size + 256)
    EndIf
    dict(dict_size) = w + Left(entry, 1)
    dict_size + 1 ;no check is performed for overfilling the dictionary.

    w = entry
  Wend
  ProcedureReturn result
EndProcedure

If OpenConsole()
  ;How to use:

  Define initial.s, decompressed.s

  Print("Type something: ")
  initial = Input()
  NewList compressed.u()
  compress(initial, compressed())
  ForEach compressed()
    Print(Str(compressed()) + " ")
  Next
  PrintN("")

  decompressed = decompress(compressed())
  PrintN(decompressed)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
