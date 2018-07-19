Procedure biased(n)
  If Random(n) <> 1
    ProcedureReturn 0
  EndIf
  ProcedureReturn 1
EndProcedure

Procedure unbiased(n)
  Protected a, b
  Repeat
    a = biased(n)
    b = biased(n)
  Until a <> b
  ProcedureReturn a
EndProcedure

#count = 100000

Define n, m, output.s
For n = 3 To 6
  Dim b_count(1)
  Dim u_count(1)
  For m = 1 To #count
    x = biased(n)
    b_count(x) + 1
    x = unbiased(n)
    u_count(x) + 1
  Next
  output + "N = " + Str(n) + #LF$
  output + "  biased =>" + #tab$ + "#0=" + Str(b_count(0)) + #tab$ + "#1=" +Str(b_count(1))
  output + #tab$ + " ratio=" + StrF(b_count(1) / #count * 100, 2) + "%" + #LF$
  output + "  unbiased =>" + #tab$ + "#0=" + Str(u_count(0)) + #tab$ + "#1=" + Str(u_count(1))
  output + #tab$ + " ratio=" + StrF(u_count(1) / #count * 100, 2) + "%" + #LF$
Next
MessageRequester("Biased and Unbiased random number results", output)
