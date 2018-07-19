Procedure spiralMatrix(size = 1)
  Protected i, x = -1, y, count = size, n
  Dim a(size - 1,size - 1)

  For i = 1 To count
    x + 1
    a(x,y) = n
    n + 1
  Next

  Repeat
    count - 1
    For i = 1 To count
      y + 1
      a(x,y) = n
      n + 1
    Next
    For i = 1 To count
      x - 1
      a(x,y) = n
      n + 1
    Next

    count - 1
    For i = 1 To count
      y - 1
      a(x,y) = n
      n + 1
    Next
    For i = 1 To count
      x + 1
      a(x,y) = n
      n + 1
    Next
  Until count < 1

  PrintN("Spiral: " + Str(Size) + #CRLF$)
  Protected colWidth = Len(Str(size * size - 1)) + 1
  For y = 0 To size - 1
    For x = 0 To size - 1
      Print("" + LSet(Str(a(x, y)), colWidth, " ") + "")
    Next
    PrintN("")
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  spiralMatrix(2)
  PrintN("")
  spiralMatrix(5)


  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
