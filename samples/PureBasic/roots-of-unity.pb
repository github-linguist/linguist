OpenConsole()
For n = 2 To 10
  angle = 0
  PrintN(Str(n))
  For i = 1 To n
    x.f = Cos(Radian(angle))
    y.f = Sin(Radian(angle))
    PrintN( Str(i) + ":  " + StrF(x, 6) +  " / " + StrF(y, 6))
    angle = angle + (360 / n)
  Next
Next
Input()
