Procedure.f Max (Array a.f(1))
   Protected last, i, ret.f

   ret = a(0)
   last = ArraySize(a())
   For i = 1 To last
      If ret < a(i)
         ret = a(i)
      EndIf
   Next

   ProcedureReturn ret
EndProcedure
