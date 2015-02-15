EnableExplicit

Procedure KnuthShuffle(Array a(1))
   Protected i, last = ArraySize(a())

   For i = last To 1 Step -1
      Swap a(i), a(Random(i))
   Next
EndProcedure

Procedure.s ArrayToString(Array a(1))
   Protected ret$, i, last = ArraySize(a())

   ret$ = Str(a(0))
   For i = 1 To last
      ret$ + "," + Str(a(i))
   Next
   ProcedureReturn ret$
EndProcedure


#NumElements = 10

Dim a(#NumElements-1)
Define i

For i = 0 To #NumElements-1
   a(i) = i
Next

KnuthShuffle(a())
Debug "shuffled: " + ArrayToString(a())
