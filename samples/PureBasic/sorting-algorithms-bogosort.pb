Procedure KnuthShuffle (Array a(1))
  Protected i, Size = ArraySize(a())
  For i = 0 To Size
    Swap a(i), a(Random(Size))
  Next
EndProcedure

Procedure isSorted(Array a(1))
  Protected i, Size = ArraySize(a())
  For i = 1 To Size
    If a(i) < a(i - 1)
      ProcedureReturn #False
    EndIf
  Next
  ProcedureReturn #True
EndProcedure

Procedure BogoSort(Array a(1))
  Protected Size = ArraySize(a()) + 1, iter

  While Not isSorted(a())
    iter + 1
    KnuthShuffle(a())
  Wend
  MessageRequester("Results","Array of " + Str(Size) + " integers required " + Str(iter) + " shuffles To SORT.")
EndProcedure

Dim b(10)
For i = 0 To 10
  b(i) = Random(100)
Next

BogoSort(b())
