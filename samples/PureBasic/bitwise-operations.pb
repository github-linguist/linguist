Procedure Bitwise(a, b)
  Debug  a & b      ; And
  Debug a | b       ;Or
  Debug a ! b       ; XOr
  Debug ~a          ;Not
  Debug a << b      ; shift left
  Debug a >> b      ; arithmetic shift right
  ; Logical shift right and rotates are not available
  ; You can of use inline ASM to achieve this:
  Define Temp
  ; logical shift right
  !mov edx, dword [p.v_a]
  !mov ecx, dword [p.v_b]
  !shr edx, cl
  !mov dword [p.v_Temp], edx
  Debug Temp
  ; rotate left
  !mov edx, dword [p.v_a]
  !mov ecx, dword [p.v_b]
  !rol edx, cl
  !mov dword [p.v_Temp], edx
  Debug Temp
  ; rotate right
  !mov edx, dword [p.v_a]
  !mov ecx, dword [p.v_b]
  !ror edx, cl
  !mov dword [p.v_Temp], edx
  Debug Temp
EndProcedure
