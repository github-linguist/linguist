Procedure displayArray(Array a(1), msg.s)
  Protected i
  Print(msg + " [")
  For i = 0 To ArraySize(a())
    Print(Str(a(i)))
    If i <> ArraySize(a())
      Print(", ")
    EndIf
  Next
  PrintN("]")
EndProcedure

Procedure randomElements(Array a(1), lo, hi)
  Protected i
  For i = 0 To ArraySize(a())
    a(i) = random(hi - lo) + lo
  Next
EndProcedure

Procedure arrayConcat(Array a(1), Array b(1), Array c(1))
  Protected i, newSize = ArraySize(a()) + ArraySize(b()) + 1
  Dim c(newSize)
  For i = 0 To ArraySize(a())
    c(i) = a(i)
  Next
  For i = 0 To ArraySize(b())
    c(i + ArraySize(a()) + 1) = b(i)
  Next
EndProcedure


If OpenConsole()
  Dim a(random(3) + 1)
  Dim b(random(3) + 1)
  Dim c(0) ;array will be resized by arrayConcat()

  randomElements(a(), -5, 5)
  randomElements(b(), -5, 5)
  displayArray(a(), "a:")
  displayArray(b(), "b:")
  arrayConcat(a(), b(), c())
  displayArray(c(), "concat of a[] + b[]:")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
