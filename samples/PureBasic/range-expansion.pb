Procedure rangeexpand(txt.s, List outputList())
  Protected rangesCount = CountString(txt, ",") + 1
  Protected subTxt.s, r, rangeMarker, rangeStart, rangeFinish, rangeIncrement, i

  LastElement(outputList())
  For r = 1 To rangesCount
    subTxt = StringField(txt, r, ",")
    rangeMarker = FindString(subTxt, "-", 2)
    If rangeMarker
      rangeStart = Val(Mid(subTxt, 1, rangeMarker - 1))
      rangeFinish = Val(Mid(subTxt, rangeMarker + 1))

      If rangeStart > rangeFinish
        rangeIncrement = -1
      Else
        rangeIncrement = 1
      EndIf

      i = rangeStart - rangeIncrement
      Repeat
        i + rangeIncrement
        AddElement(outputList()): outputList() = i
      Until i = rangeFinish
    Else
      AddElement(outputList()): outputList() = Val(subTxt)
    EndIf
  Next
EndProcedure

Procedure outputListValues(List values())
  Print("[ ")
  ForEach values()
    Print(Str(values()) + " ")
  Next
  PrintN("]")
EndProcedure

If OpenConsole()
  NewList values()
  rangeexpand("-6,-3--1,3-5,7-11,14,15,17-20", values())
  outputListValues(values())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
