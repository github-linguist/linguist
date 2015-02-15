Procedure display(List m())
  ForEach m()
    Print(LSet(Str(m()), 3," "))
  Next
  PrintN("")
EndProcedure

;overwrites list m() with the merger of lists ma() and mb()
Procedure merge(List m(), List ma(), List mb())
  FirstElement(m())
  Protected ma_elementExists = FirstElement(ma())
  Protected mb_elementExists = FirstElement(mb())
  Repeat
    If ma() <= mb()
      m() = ma(): NextElement(m())
      ma_elementExists = NextElement(ma())
    Else
      m() = mb(): NextElement(m())
      mb_elementExists = NextElement(mb())
    EndIf
  Until Not (ma_elementExists And mb_elementExists)

  If ma_elementExists
    Repeat
      m() = ma(): NextElement(m())
    Until Not NextElement(ma())
  ElseIf mb_elementExists
    Repeat
      m() = mb(): NextElement(m())
    Until Not NextElement(mb())
  EndIf
EndProcedure

Procedure mergesort(List m())
  Protected NewList ma()
  Protected NewList mb()

  If ListSize(m()) > 1
    Protected current, middle = (ListSize(m()) / 2 ) - 1

    FirstElement(m())
    While current <= middle
      AddElement(ma())
      ma() = m()
      NextElement(m()): current + 1
    Wend

    PreviousElement(m())
    While NextElement(m())
      AddElement(mb())
      mb() = m()
    Wend

    mergesort(ma())
    mergesort(mb())
    LastElement(ma()): FirstElement(mb())
    If ma() <= mb()
      FirstElement(m())
      FirstElement(ma())
      Repeat
        m() = ma(): NextElement(m())
      Until Not NextElement(ma())
      Repeat
        m() = mb(): NextElement(m())
      Until Not NextElement(mb())
    Else
      merge(m(), ma(), mb())
    EndIf
  EndIf
EndProcedure

If OpenConsole()
  Define i
  NewList x()

  For i = 1 To 21: AddElement(x()): x() = Random(60): Next
  display(x())
  mergesort(x())
  display(x())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
