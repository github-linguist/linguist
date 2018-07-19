Define.f
If OpenConsole()
  inf = 1/None
  minus_inf  = -1/None
  minus_zero = -1/inf
  nan = None/None

  PrintN("positive infinity: "+StrF(inf))
  PrintN("negative infinity: "+StrF(minus_inf))
  PrintN("positive zero: "+StrF(None))
  PrintN("negative zero: "+StrF(minus_zero)) ; handles as 0.0
  PrintN("not a number: "+StrF(nan))
  PrintN("Arithmetics")
  PrintN("+inf + 2.0 =  "+StrF(inf + 2.0))
  PrintN("+inf - 10.1 = "+StrF(inf - 10.1))
  PrintN("+inf + -inf = "+StrF(inf + minus_inf))
  PrintN("0.0 * +inf =  "+StrF(0.0 * inf))
  PrintN("1.0/-0.0 =  "+StrF(1.0/minus_zero))
  PrintN("NaN + 1.0 = "+StrF(nan + 1.0))
  PrintN("NaN + NaN = "+StrF(nan + nan))
  PrintN("Logics")
  If IsInfinity(inf): PrintN("Variabel 'Infinity' is infinite"): EndIf
  If IsNAN(nan): PrintN("Variable 'nan' is not a number"): EndIf

  Print(#CRLF$+"Press ENTER to EXIT"): Input()
EndIf
