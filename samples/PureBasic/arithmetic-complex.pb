Structure Complex
  real.d
  imag.d
EndStructure

Procedure Add_Complex(*A.Complex, *B.Complex)
  Protected *R.Complex=AllocateMemory(SizeOf(Complex))
  If *R
    *R\real=*A\real+*B\real
    *R\imag=*A\imag+*B\imag
  EndIf
  ProcedureReturn *R
EndProcedure

Procedure Inv_Complex(*A.Complex)
  Protected *R.Complex=AllocateMemory(SizeOf(Complex)), denom.d
  If *R
    denom  = *A\real * *A\real + *A\imag * *A\imag
    *R\real= *A\real / denom
    *R\imag=-*A\imag / denom
  EndIf
  ProcedureReturn *R
EndProcedure

Procedure Mul_Complex(*A.Complex, *B.Complex)
  Protected *R.Complex=AllocateMemory(SizeOf(Complex))
  If *R
    *R\real=*A\real * *B\real - *A\imag * *B\imag
    *R\imag=*A\real * *B\imag + *A\imag * *B\real
  EndIf
  ProcedureReturn *R
EndProcedure

Procedure Neg_Complex(*A.Complex)
  Protected *R.Complex=AllocateMemory(SizeOf(Complex))
  If *R
    *R\real=-*A\real
    *R\imag=-*A\imag
  EndIf
  ProcedureReturn *R
EndProcedure

Procedure ShowAndFree(Header$, *Complex.Complex)
  If *Complex
    Protected.d i=*Complex\imag, r=*Complex\real
    Print(LSet(Header$,7))
    Print("= "+StrD(r,3))
    If i>=0:  Print(" + ")
    Else:     Print(" - ")
    EndIf
    PrintN(StrD(Abs(i),3)+"i")
    FreeMemory(*Complex)
  EndIf
EndProcedure

If OpenConsole()
  Define.Complex a, b, *c
  a\real=1.0: a\imag=1.0
  b\real=#PI: b\imag=1.2
  *c=Add_Complex(a,b):  ShowAndFree("a+b",    *c)
  *c=Mul_Complex(a,b):  ShowAndFree("a*b",    *c)
  *c=Inv_Complex(a):    ShowAndFree("Inv(a)", *c)
  *c=Neg_Complex(a):    ShowAndFree("-a",     *c)
  Print(#CRLF$+"Press ENTER to exit"):Input()
EndIf
