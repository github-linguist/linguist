Procedure.f RandomNormal()
   ; This procedure can return any real number.
   Protected.f x1, x2

   ; random numbers from the open interval ]0, 1[
   x1 = (Random(999998)+1) / 1000000       ; must be > 0 because of Log(x1)
   x2 = (Random(999998)+1) / 1000000

   ProcedureReturn Sqr(-2*Log(x1)) * Cos(2*#PI*x2)
EndProcedure


Define i, n=1000

Dim a.q(n-1)
For i = 0 To n-1
   a(i) = 1 + 0.5 * RandomNormal()
Next
