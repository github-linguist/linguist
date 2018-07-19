Procedure.i IsPrime(n)
   Protected k

   If n = 2
      ProcedureReturn #True
   EndIf

   If n <= 1 Or n % 2 = 0
      ProcedureReturn #False
   EndIf

   For k = 3 To Int(Sqr(n)) Step 2
      If n % k = 0
         ProcedureReturn #False
      EndIf
   Next

   ProcedureReturn #True
EndProcedure
