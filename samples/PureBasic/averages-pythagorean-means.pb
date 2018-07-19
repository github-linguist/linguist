Procedure.d ArithmeticMean()
  For a = 1 To 10
    mean + a
  Next
  ProcedureReturn mean / 10
EndProcedure
Procedure.d GeometricMean()
  mean = 1
  For a = 1 To 10
    mean * a
  Next
  ProcedureReturn Pow(mean, 1 / 10)
EndProcedure
Procedure.d HarmonicMean()
  For a = 1 To 10
    mean.d + 1 / a
  Next
  ProcedureReturn 10 / mean
EndProcedure

If HarmonicMean() <= GeometricMean() And GeometricMean() <= ArithmeticMean()
  Debug "true"
EndIf
Debug ArithmeticMean()
Debug GeometricMean()
Debug HarmonicMean()
