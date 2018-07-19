Procedure.s RepeatString(count, text$=" ")
   Protected i, ret$=""

   For i = 1 To count
      ret$ + text$
   Next
   ProcedureReturn ret$
EndProcedure

Debug RepeatString(5, "ha")
