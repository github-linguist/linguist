Prototype RandNum_prt()

Procedure.s distcheck(*function.RandNum_prt, repetitions, delta.d)
  Protected text.s, maxIndex = 0
  Dim bucket(maxIndex) ;array will be resized as needed

  For i = 1 To repetitions ;populate buckets
    v = *function()
    If v > maxIndex
      maxIndex = v
      Redim bucket(maxIndex)
    EndIf
    bucket(v) + 1
  Next


  lbnd = Round((repetitions / maxIndex) * (100 - delta) / 100, #PB_Round_Up)
  ubnd = Round((repetitions / maxIndex) * (100 + delta) / 100, #PB_Round_Down)
  text = "Distribution check:" + #crlf$ + #crlf$
  text + "Total elements = " + Str(repetitions) + #crlf$ + #crlf$
  text + "Margin = " + StrF(delta, 2) + "% --> Lbound = " + Str(lbnd) + ", Ubound = " + Str(ubnd) + #crlf$

  For i = 1 To maxIndex
    If bucket(i) < lbnd Or bucket(i) > ubnd
      text + #crlf$ + "Bucket " + Str(i) + " contains " + Str(bucket(i)) + " elements.  Skewed."
    EndIf
  Next
  ProcedureReturn text
EndProcedure

MessageRequester("Results", distcheck(@dice7(), 1000000, 0.20))
