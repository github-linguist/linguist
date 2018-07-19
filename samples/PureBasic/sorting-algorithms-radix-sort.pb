Structure bucket
  List i.i()
EndStructure

DataSection
  ;sets specify the size (1 based) followed by each integer
  set1:
  Data.i 10 ;size
  Data.i 1, 3, 8, 9, 0, 0, 8, 7, 1, 6 ;data
  set2:
  Data.i 8
  Data.i 170, 45, 75, 90, 2, 24, 802, 66
  set3:
  Data.i 8
  Data.i 170, 45, 75, 90, 2, 24, -802, -66
EndDataSection

Procedure setIntegerArray(Array x(1), *setPtr)
  Protected i, count
  count = PeekI(*setPtr) - 1 ;convert to zero based count
  *setPtr + SizeOf(Integer) ;move pointer forward to data
  Dim x(count)
  For i = 0 To count
    x(i) = PeekI(*setPtr + i * SizeOf(Integer))
  Next
EndProcedure

Procedure displayArray(Array x(1))
  Protected i, Size = ArraySize(x())
  For i = 0 To Size
    Print(Str(x(i)))
    If i < Size: Print(", "): EndIf
  Next
  PrintN("")
EndProcedure

Procedure radixSort(Array x(1), Base = 10)
  Protected count = ArraySize(x())
  If Base < 1 Or count < 1: ProcedureReturn: EndIf ;exit due to invalid values

  Protected i, pv, digit, digitCount, maxAbs, pass, index
  ;find element with largest number of digits
  For i = 0 To count
    If Abs(x(i)) > maxAbs
      maxAbs = Abs(x(i))
    EndIf
  Next

  digitCount = Int(Log(maxAbs)/Log(Base)) + 1

  For pass = 1 To digitCount
    Dim sortBuckets.bucket(Base * 2 - 1)
    pv = Pow(Base, pass - 1)

    ;place elements in buckets according to the current place-value's digit
    For index = 0 To count
      digit = Int(x(index)/pv) % Base + Base
      AddElement(sortBuckets(digit)\i())
      sortBuckets(digit)\i() = x(index)
    Next

    ;transfer contents of buckets back into array
    index = 0
    For digit = 1 To (Base * 2) - 1
      ForEach sortBuckets(digit)\i()
        x(index) = sortBuckets(digit)\i()
        index + 1
      Next
    Next
  Next
EndProcedure

If OpenConsole()
  Dim x(0)
  setIntegerArray(x(), ?set1)
  radixSort(x()): displayArray(x())

  setIntegerArray(x(), ?set2)
  radixSort(x()): displayArray(x())

  setIntegerArray(x(), ?set3)
  radixSort(x(), 2): displayArray(x())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
