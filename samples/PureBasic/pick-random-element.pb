Procedure.s pickRandomElement(List source.s())
  Protected x = ListSize(source())

  If x > 0
    SelectElement(source(), Random(x - 1)) ;element numbering is zero - based
    ProcedureReturn source()
  EndIf
EndProcedure

;initialize list elements
DataSection
  elements:
  Data.s "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"
EndDataSection

#elementCount = 10
NewList item.s()

Restore elements
Define i
For i = 1 To #elementCount
  AddElement(item())
  Read.s item()
Next

If OpenConsole()
  Print("Source list:  ")
  ForEach item()
    Print(item() + " ")
  Next
  PrintN(#CRLF$)

  Print("Random picks from list:  ")
  For i = 1 To 10
    Print(pickRandomElement(item()) + " ")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
