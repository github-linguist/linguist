Structure vector
  x.f
  y.f
  z.f
EndStructure

;convert vector to a string for display
Procedure.s toString(*v.vector)
  ProcedureReturn "[" + StrF(*v\x, 2) + ", " + StrF(*v\y, 2) + ", " + StrF(*v\z, 2) + "]"
EndProcedure

Procedure.f dotProduct(*a.vector, *b.vector)
  ProcedureReturn *a\x * *b\x + *a\y * *b\y + *a\z * *b\z
EndProcedure

Procedure crossProduct(*a.vector, *b.vector, *r.vector)
  *r\x = *a\y * *b\z - *a\z * *b\y
  *r\y = *a\z * *b\x - *a\x * *b\z
  *r\z = *a\x * *b\y - *a\y * *b\x
EndProcedure

Procedure.f scalarTriple(*a.vector, *b.vector, *c.vector)
  Protected r.vector
  crossProduct(*b, *c, r)
  ProcedureReturn dotProduct(*a, r)
EndProcedure

Procedure vectorTriple(*a.vector, *b.vector, *c.vector, *r.vector)
  Protected r.vector
  crossProduct(*b, *c, r)
  crossProduct(*a, r, *r)
EndProcedure

If OpenConsole()
  Define.vector a, b, c, r
  a\x = 3: a\y = 4: a\z = 5
  b\x = 4: b\y = 3: b\z = 5
  c\x = -5: c\y = -12: c\z = -13

  PrintN("a = " + toString(a) + ", b = " + toString(b) + ", c = " + toString(c))
  PrintN("a . b = " + StrF(dotProduct(a, b), 2))
  crossProduct(a, b, r)
  PrintN("a x b = " + toString(r))
  PrintN("a . b x c  = " + StrF(scalarTriple(a, b, c), 2))
  vectorTriple(a, b, c, r)
  PrintN("a x b x c = " + toString(r))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
