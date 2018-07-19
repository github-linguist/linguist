; Allocate a 1Mb memory area work within to avoid conflicts,
; this address could be any number but it may then fail on some systems.
*a=AllocateMemory(1024*1024)

; Write a int wit value "31415" at address +312,
; using pointer '*a' with a displacement.
PokeI(*a+312, 31415)

; Write a float with value Pi at address +316,
; by creating a new pointer '*b' for this address
*b=*a+316
PokeF(*b, #PI)

;Now test it
For i=0 To 1024000 Step 4
  n=PeekI(*a+i)
  If n
    Debug "Int at +"+Str(i)+"  = "+Str(n)
    Debug "Float at +"+Str(i)+"= "+StrF(PeekF(*a+i))
  EndIf
Next
