Procedure nextCombination(Array combIndex(1), elementCount)
  ;combIndex() must be dimensioned to 'k' - 1, elementCount equals 'n' - 1
  ;combination produced includes repetition of elements and is represented by the array combIndex()
  Protected i, indexValue, combSize = ArraySize(combIndex()), curIndex

  ;update indexes
  curIndex = combSize
  Repeat
    combIndex(curIndex) + 1
    If combIndex(curIndex) > elementCount

      curIndex - 1
      If curIndex < 0
        For i = 0 To combSize
          combIndex(i) = 0
        Next
        ProcedureReturn #False ;array reset to first combination
      EndIf

    ElseIf curIndex < combSize

      indexValue = combIndex(curIndex)
      Repeat
        curIndex + 1
        combIndex(curIndex) = indexValue
      Until curIndex = combSize

    EndIf
  Until  curIndex = combSize

  ProcedureReturn #True ;array contains next combination
EndProcedure

Procedure.s display(Array combIndex(1), Array dougnut.s(1))
  Protected i, elementCount = ArraySize(combIndex()), output.s = "  "
  For i = 0 To elementCount
    output + dougnut(combIndex(i)) + " + "
  Next
  ProcedureReturn Left(output, Len(output) - 3)
EndProcedure

DataSection
  Data.s "iced", "jam", "plain"
EndDataSection

If OpenConsole()
  Define n = 3, k = 2, i, combinationCount
  Dim combIndex(k - 1)
  Dim dougnut.s(n - 1)
  For i = 0 To n - 1: Read.s dougnut(i): Next

  PrintN("Combinations of " + Str(k) + " dougnuts taken " + Str(n) + " at a time with repetitions.")
  combinationCount = 0
  Repeat
    PrintN(display(combIndex(), dougnut()))
    combinationCount + 1
  Until Not nextCombination(combIndex(), n - 1)
  PrintN("Total combination count: " + Str(combinationCount))

  ;extra credit
  n = 10: k = 3
  Dim combIndex(k - 1)
  combinationCount = 0
  Repeat: combinationCount + 1: Until Not nextCombination(combIndex(), n - 1)
  PrintN(#CRLF$ + "Ways to select " + Str(k) + " items from " + Str(n) + " types: " + Str(combinationCount))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
