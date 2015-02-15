DataSection
  Array_1:
  Data.i 5              ;element count
  Data.i 1, 2, 3, 4, 5  ;element data
  Array_2:
  Data.i 6
  Data.i 1, 2, 1, 5, 2, 2
  Array_3:
  Data.i 5
  Data.i 1, 2, 1, 5, 2
  Array_4:
  Data.i 5
  Data.i 1, 2, 1, 5, 2
  Array_5:
  Data.i 4
  Data.i 1, 2, 1, 6
  Array_6:
  Data.i 5
  Data.i 1, 2, 1, 6, 2
EndDataSection

#False = 0
#True = 1

;helper subrountine to initialize a dataset, *dataPtr points to the elementcount followed by the element data
Procedure initArrayData(Array a(1), *dataPtr)
  Protected elementCount = PeekI(*dataPtr)

  Dim a(elementCount - 1)
  For i = 0 To elementCount - 1
    *dataPtr + SizeOf(Integer)
    a(i) = PeekI(*dataPtr)
  Next
EndProcedure

;helper subroutine that returns 'True' or 'False' for a boolean input
Procedure.s booleanText(b)
  If b: ProcedureReturn "True": EndIf
  ProcedureReturn "False"
EndProcedure

Procedure order(Array a(1), Array b(1))
  Protected len_a = ArraySize(a()), len_b = ArraySize(b()), elementIndex

  While elementIndex <= len_a And elementIndex <= len_b And a(elementIndex) = b(elementIndex)
    elementIndex + 1
  Wend

  If (elementIndex > len_a  And elementIndex <= len_b) Or (elementIndex <= len_b And a(elementIndex) <= b(elementIndex))
    ProcedureReturn #True
  EndIf
EndProcedure

Dim A_1(0): initArrayData(A_1(), ?Array_1)
Dim A_2(0): initArrayData(A_2(), ?Array_2)
Dim A_3(0): initArrayData(A_3(), ?Array_3)
Dim A_4(0): initArrayData(A_4(), ?Array_4)
Dim A_5(0): initArrayData(A_5(), ?Array_5)
Dim A_6(0): initArrayData(A_6(), ?Array_6)

If OpenConsole()
  PrintN(booleanText(order(A_1(), A_2()))) ;False
  PrintN(booleanText(order(A_2(), A_3()))) ;False
  PrintN(booleanText(order(A_3(), A_4()))) ;False
  PrintN(booleanText(order(A_4(), A_5()))) ;True
  PrintN(booleanText(order(A_5(), A_6()))) ;True

  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
