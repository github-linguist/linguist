Procedure forward_difference(List a())
  If ListSize(a()) <= 1
    ClearList(a()): ProcedureReturn
  EndIf
  Protected NewList b()
  CopyList(a(), b())
  LastElement(a()): DeleteElement(a())
  SelectElement(b(), 1)
  ForEach a()
    a() - b(): NextElement(b())
  Next
EndProcedure

Procedure nth_difference(List a(), List b(), n)
  Protected i
  CopyList(a(), b())
  For i = 1 To n
    forward_difference(b())
  Next
EndProcedure

Procedure.s display(List a())
  Protected output.s
  ForEach a()
    output + Str(a()) + ","
  Next
  ProcedureReturn RTrim(output,",")
EndProcedure

DataSection
  ;list data
  Data.i 10 ;element count
  Data.i 90, 47, 58, 29, 22, 32, 55, 5, 55, 73
EndDataSection

;create and fill list
Define i
NewList a()
Read.i i
While i > 0
  AddElement(a()): Read.i a(): i - 1
Wend

If OpenConsole()
  NewList b()
  For i = 1 To 10
    nth_difference(a(), b(), i)
    PrintN(Str(i) + "   [" + display(b()) + "]")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
