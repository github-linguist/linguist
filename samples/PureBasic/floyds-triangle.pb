Procedure.i sumTo(n)
  Protected r,i
  For i=1 To n
    r+i
  Next
  ProcedureReturn r.i
EndProcedure

; [1]
; array rsA(n)... string-lengths of the numbers
; in the bottom row

; [2]
; sumTo(i-1)+1    to     sumTo(i)
           ; 11 12 13 14 15
  ; here k is the column-index for array rsA(k)

Procedure.s FloydsTriangle(n)
  Protected r.s,s.s,t.s,i,j,k
  ; [1]
  Dim rsA(n)
  i=0
  For j=sumTo(n-1)+1 To sumTo(n)
    i+1
    rsA(i)=Len(Str(j))
  Next
  ; [2]
  For i=1 To n
    t.s="":k=0
    For j=sumTo(i-1)+1 To sumTo(i)
      k+1:t.s+RSet(Str(j),rsA(k)," ")+" "
    Next
    r.s+RTrim(t.s)+Chr(13)+Chr(10)
  Next
  r.s=Left(r.s,Len(r.s)-2)
  ProcedureReturn r.s
EndProcedure

If OpenConsole()
  n=5
  r.s=FloydsTriangle(n)
  PrintN(r.s)

  n=14
  r.s=FloydsTriangle(n)
  PrintN(r.s)

  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
