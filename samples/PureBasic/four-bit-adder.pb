;Because no representation for a solitary bit is present, bits are stored as bytes.
;Output values from the constructive building blocks is done using pointers (i.e. '*').

Procedure.b notGate(x)
  ProcedureReturn ~x
EndProcedure

Procedure.b xorGate(x,y)
  ProcedureReturn  (x & notGate(y)) | (notGate(x) & y)
EndProcedure

Procedure halfadder(a, b, *sum.Byte, *carry.Byte)
  *sum\b = xorGate(a, b)
  *carry\b = a & b
EndProcedure

Procedure fulladder(a, b, c0, *sum.Byte, *c1.Byte)
  Protected sum_ac.b, carry_ac.b, carry_sb.b

  halfadder(c0, a, @sum_ac, @carry_ac)
  halfadder(sum_ac, b, *sum, @carry_sb)
  *c1\b = carry_ac | carry_sb
EndProcedure

Procedure fourbitsadder(a0, a1, a2, a3, b0, b1, b2, b3 , *s0.Byte, *s1.Byte, *s2.Byte, *s3.Byte, *v.Byte)
  Protected.b c1, c2, c3

  fulladder(a0, b0, 0,   *s0, @c1)
  fulladder(a1, b1, c1,  *s1, @c2)
  fulladder(a2, b2, c2,  *s2, @c3)
  fulladder(a3, b3, c3,  *s3, *v)
EndProcedure

;// Test implementation, map two 4-character strings to the inputs of the fourbitsadder() and display results
Procedure.s test_4_bit_adder(a.s,b.s)
  Protected.b s0, s1, s2, s3,  v, i
  Dim a.b(3)
  Dim b.b(3)
  For i = 0 To 3
    a(i) = Val(Mid(a, 4 - i, 1))
    b(i) = Val(Mid(b, 4 - i, 1))
  Next

  fourbitsadder(a(0), a(1), a(2), a(3), b(0), b(1), b(2), b(3), @s0, @s1, @s2, @s3, @v)
  ProcedureReturn a + " + " + b + " = " + Str(s3) + Str(s2) + Str(s1) + Str(s0) + " overflow " + Str(v)
EndProcedure

If OpenConsole()
  PrintN(test_4_bit_adder("0110","1110"))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
