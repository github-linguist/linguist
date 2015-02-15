Procedure.d f(x.d)
  ProcedureReturn Pow(Abs(x), 0.5) + 5 * x * x * x
EndProcedure

Procedure split(i.s, delimeter.s, List o.d())
  Protected index = CountString(i, delimeter) + 1 ;add 1 because last entry will not have a delimeter

  While index > 0
    AddElement(o())
    o() = ValD(Trim(StringField(i, index, delimeter)))
    index - 1
  Wend

  ProcedureReturn ListSize(o())
EndProcedure

Define i$, entriesAreValid = 0, result.d, output$
NewList numbers.d()

If OpenConsole()
  Repeat
    PrintN(#crlf$ + "Enter eleven numbers that are each separated by spaces or commas:")

    i$ = Input(
    i$ = Trim(i$)
    If split(i$, ",", numbers.d()) < 11
      ClearList(numbers())
      If split(i$, " ", numbers.d()) < 11
        PrintN("Not enough numbers were supplied.")
        ClearList(numbers())
      Else
        entriesAreValid = 1
      EndIf
    Else
      entriesAreValid = 1
    EndIf
  Until entriesAreValid = 1

  ForEach numbers()
    output$ = "f(" + RTrim(RTrim(StrD(numbers(), 3), "0"), ".") + ") = "
    result.d = f(numbers())
    If result > 400
      output$ + "Too Large"
    Else
      output$ + RTrim(RTrim(StrD(result, 3), "0"), ".")
    EndIf
    PrintN(output$)
  Next

  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
