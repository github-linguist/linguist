Procedure pascaltriangle( n.i)

  For i=  0 To  n
       c = 1
       For k=0 To i
             Print(Str( c)+" ")
         c = c * (i-k)/(k+1);
        Next ;k
    PrintN(" "); nächste zeile
  Next ;i

EndProcedure

OpenConsole()
Parameter.i = Val(ProgramParameter(0))
pascaltriangle(Parameter);
Input()
