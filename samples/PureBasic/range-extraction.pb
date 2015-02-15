DataSection
  Data.i  33 ;count of elements to be read
  Data.i  0,  1,  2,  4,  6,  7,  8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24
  Data.i  25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39
EndDataSection

NewList values()
;setup list
Define elementCount, i
Read.i elementCount
For i = 1 To elementCount
  AddElement(values()): Read.i values()
Next

Procedure.s rangeExtract(List values())
  Protected listSize = ListSize(values()) - 1
  Protected rangeMarker, rangeStart, rangeIncrement, retraceSteps, rangeSize, endOfRange, output.s, sub.s

  ForEach values()
    rangeStart = values():
    sub = Str(rangeStart)
    If NextElement(values())
      retraceSteps = 1
      rangeIncrement = values() - rangeStart
      If rangeIncrement = 1 Or rangeIncrement = -1
        ;found start of possible range
        If ListIndex(values()) <> listSize
          retraceSteps = 2
          rangeSize = 2
          endOfRange = #False
          rangeMarker = values()
          While NextElement(values())
            If values() - rangeMarker <> rangeIncrement
              endOfRange = #True
              Break
            EndIf
            rangeSize + 1
            rangeMarker = values()
          Wend

          If rangeSize > 2
            sub = Str(rangeStart) + "-" + Str(rangeMarker)
            If Not endOfRange
              retraceSteps = 0 ;at end of list
            Else
              retraceSteps = 1
            EndIf
          EndIf
        EndIf
      EndIf

      ;return to the value before look-aheads
      While retraceSteps > 0
        PreviousElement(values()): retraceSteps - 1
      Wend
    EndIf

    output + sub + ","
  Next

  ProcedureReturn RTrim(output, ",")
EndProcedure

If OpenConsole()
  PrintN(rangeExtract(values()))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
