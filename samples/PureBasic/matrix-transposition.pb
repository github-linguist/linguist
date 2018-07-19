Procedure transposeMatrix(Array a(2), Array trans(2))
  Protected rows, cols

  Protected ar = ArraySize(a(), 1) ;rows in original matrix
  Protected ac = ArraySize(a(), 2) ;cols in original matrix

  ;size the matrix receiving the transposition
  Dim trans(ac, ar)

  ;copy the values
  For rows = 0 To ar
    For cols = 0 To ac
      trans(cols, rows) = a(rows, cols)
    Next
  Next
EndProcedure

Procedure displayMatrix(Array a(2), text.s = "")
  Protected i, j
  Protected cols = ArraySize(a(), 2), rows = ArraySize(a(), 1)

  PrintN(text + ": (" + Str(rows + 1) + ", " + Str(cols + 1) + ")")
  For i = 0 To rows
    For j = 0 To cols
      Print(LSet(Str(a(i, j)), 4, " "))
    Next
    PrintN("")
  Next
  PrintN("")
EndProcedure

;setup a matrix of arbitrary size
Dim m(random(5), random(5))

Define rows, cols
;fill matrix with 'random' data
For rows = 0 To ArraySize(m(),1)      ;ArraySize() can take a dimension as its second argument
  For cols = 0 To ArraySize(m(), 2)
    m(rows, cols) = random(10) - 10
  Next
Next

Dim t(0,0) ;this will be resized during transposition
If OpenConsole()
  displayMatrix(m(), "matrix before transposition")
  transposeMatrix(m(), t())
  displayMatrix(t(), "matrix after transposition")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
