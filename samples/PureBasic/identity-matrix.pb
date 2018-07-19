>Procedure identityMatrix(Array i(2), size) ;valid only for size >= 0
  ;formats array i() as an identity matrix of size x size
  Dim i(size - 1, size - 1)

  Protected j
  For j = 0 To size - 1
    i(j, j) = 1
  Next
EndProcedure


Procedure displayMatrix(Array a(2))
  Protected rows = ArraySize(a(), 2), columns = ArraySize(a(), 1)
  Protected i, j

  For i = 0 To rows
    For j = 0 To columns
      Print(RSet(Str(a(i, j)), 3, " "))
    Next
    PrintN("")
  Next
EndProcedure

If OpenConsole()
  Dim i3(0, 0)
  Dim i4(0, 0)

  identityMatrix(i3(), 3)
  identityMatrix(i4(), 4)

  displayMatrix(i3())
  PrintN("")
  displayMatrix(i4())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
