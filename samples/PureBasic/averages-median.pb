Procedure.d median(Array values.d(1), length.i)
  If length = 0 : ProcedureReturn 0.0 : EndIf
  SortArray(values(), #PB_Sort_Ascending)
  If length % 2
    ProcedureReturn values(length / 2)
  EndIf
  ProcedureReturn 0.5 * (values(length / 2 - 1) + values(length / 2))
EndProcedure

Procedure.i readArray(Array values.d(1))
  Protected length.i, i.i
  Read.i length
  ReDim values(length - 1)
  For i = 0 To length - 1
    Read.d values(i)
  Next
  ProcedureReturn i
EndProcedure

Dim floats.d(0)
Restore array1
length.i = readArray(floats())
Debug median(floats(), length)
Restore array2
length.i = readArray(floats())
Debug median(floats(), length)

DataSection
  array1:
    Data.i 7
    Data.d 4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2
  array2:
    Data.i 6
    Data.d 4.1, 7.2, 1.7, 9.3, 4.4, 3.2
EndDataSection
