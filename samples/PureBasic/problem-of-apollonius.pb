Structure Circle
  XPos.f
  YPos.f
  Radius.f
EndStructure

Procedure ApolloniusSolver(*c1.Circle,*c2.Circle,*c3.Circle, s1, s2, s3)
  Define.f  ; This tells the compiler that all non-specified new variables
            ; should be of float type (.f).
  x1=*c1\XPos:  y1=*c1\YPos:  r1=*c1\Radius
  x2=*c2\XPos:  y2=*c2\YPos:  r2=*c2\Radius
  x3=*c3\XPos:  y3=*c3\YPos:  r3=*c3\Radius

  v11 = 2*x2 - 2*x1
  v12 = 2*y2 - 2*y1
  v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2
  v14 = 2*s2*r2 - 2*s1*r1

  v21 = 2*x3 - 2*x2
  v22 = 2*y3 - 2*y2
  v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3
  v24 = 2*s3*r3 - 2*s2*r2

  w12 = v12/v11
  w13 = v13/v11
  w14 = v14/v11

  w22 = v22/v21-w12
  w23 = v23/v21-w13
  w24 = v24/v21-w14

  P = -w23/w22
  Q =  w24/w22
  M = -w12*P-w13
  N =  w14-w12*Q

  a = N*N + Q*Q - 1
  b = 2*M*N - 2*N*x1 + 2*P*Q - 2*Q*y1 + 2*s1*r1
  c = x1*x1 + M*M - 2*M*x1 + P*P + y1*y1 - 2*P*y1 - r1*r1

  D= b*b - 4*a*c

  Define *result.Circle=AllocateMemory(SizeOf(Circle))
  ; Allocate memory for a returned Structure of type Circle.
  ; This memory should be freed later but if not, PureBasic’s
  ; internal framework will do so when the program shuts down.
  If *result
    *result\Radius=(-b-Sqr(D))/(2*a)
    *result\XPos  =M+N * *result\Radius
    *result\YPos  =P+Q * *result\Radius
  EndIf
  ProcedureReturn *result ; Sending back a pointer
EndProcedure

If OpenConsole()
  Define.Circle c1, c2, c3
  Define *c.Circle  ; '*c' is defined as a pointer to a circle-structure.
  c1\Radius=1
  c2\XPos=4:  c2\Radius=1
  c3\XPos=2:  c3\YPos=4:  c3\Radius=2

  *c=ApolloniusSolver(@c1, @c2, @c3, 1, 1, 1)
  If *c ; Verify that *c got allocated
    PrintN("Circle [x="+StrF(*c\XPos,2)+", y="+StrF(*c\YPos,2)+", r="+StrF(*c\Radius,2)+"]")
    FreeMemory(*c)  ; We are done with *c for the first calculation
  EndIf

  *c=ApolloniusSolver(@c1, @c2, @c3,-1,-1,-1)
  If *c
    PrintN("Circle [x="+StrF(*c\XPos,2)+", y="+StrF(*c\YPos,2)+", r="+StrF(*c\Radius,2)+"]")
    FreeMemory(*c)
  EndIf
  Print("Press ENTER to exit"): Input()
EndIf
