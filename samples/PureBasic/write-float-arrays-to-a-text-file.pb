#Size = 4

DataSection
  Data.f 1, 2, 3, 1e11 ;x values, how many values needed is determined by #Size
EndDataSection

Dim x.f(#Size - 1)
Dim y.f(#Size - 1)

Define i
For i = 0 To #Size - 1
  Read.f x(i)
  y(i) = Sqr(x(i))
Next

Define file$, fileID, xprecision = 3, yprecision = 5, output$

file$ = SaveFileRequester("Text file for float data", "xydata.txt","Text file | *.txt", 0)
If file$
  fileID = OpenFile(#PB_Any, file$)
  If fileID
    For i = 0 To #Size - 1
      output$ = StrF(x(i), xprecision) + Chr(9) + StrF(y(i), yprecision)
      WriteStringN(fileID, output$)
    Next
    CloseFile(fileID)
  EndIf
EndIf
