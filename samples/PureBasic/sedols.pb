Procedure.s SEDOLs(rawstring$)
  Protected i, j, sum, c, m
  For i=1 To Len(rawstring$)
    c=Asc(Mid(rawstring$,i,1))
    Select c
      Case Asc("0") To Asc("9")
        j=Val(Mid(rawstring$,i,1))
      Default
        j=c-Asc("A")
    EndSelect
    Select i
      Case 1, 3, 7:  m=1
      Case 2, 5:     m=3
      Case 4:        m=7
      Default:       m=9
    EndSelect
    sum+j*m
  Next
  sum=(10-(sum%10))%10
  ProcedureReturn rawstring$+Str(sum)
EndProcedure

Define result$, i
Restore Tests
For i=0 To 10
  Read.s  SEDOL$
  result$+SEDOLs(SEDOL$)
  If i%2
    result$+#CRLF$
  ElseIf i<10
    result$+", "
  EndIf
Next
MessageRequester("SEDOLs","Result"+#CRLF$+result$)

DataSection
  Tests:
  Data.s  "710889","B0YBKJ","406566","B0YBLH","228276"
  Data.s  "B0YBKL","557910","B0YBKR","585284","B0YBKT","B00030"
EndDataSection
