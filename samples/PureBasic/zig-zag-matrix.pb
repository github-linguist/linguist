Procedure zigZag(size)
  Protected i, v, x, y

  Dim a(size - 1, size - 1)

  x = 1
  y = 1
  For i = 1 To  size * size  ;loop once for each element
    a(x - 1, y - 1) = v      ;assign the next index

    If (x + y) & 1 = 0       ;even diagonal (zero based count)
      If x < size            ;while inside the square
        If y > 1             ;move right-up
          y - 1
        EndIf
        x + 1
      Else
        y + 1                ;on the edge increment y, but not x until diagonal is odd
      EndIf
    Else                     ;odd diagonal (zero based count)
      If y < size            ;while inside the square
        If x > 1             ;move left-down
          x - 1
        EndIf
        y + 1
      Else
        x + 1                ;on the edge increment x, but not y until diagonal is even
      EndIf
    EndIf
    v + 1
  Next


  ;generate and show printout
  PrintN("Zig-zag matrix of size " + Str(size) + #CRLF$)
  maxDigitCount = Len(Str(size * size)) + 1
  For y = 0 To size - 1
    For x = 0 To size - 1
      Print(RSet(Str(a(x, y)), maxDigitCount, " "))
    Next
    PrintN("")
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  zigZag(5)
  zigZag(6)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
