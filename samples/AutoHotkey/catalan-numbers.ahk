Loop 15
   out .= "`n" Catalan(A_Index)
Msgbox % clipboard := SubStr(out, 2)
catalan( n ) {
; By [VxE]. Returns ((2n)! / ((n + 1)! * n!)) if 0 <= N <= 22 (higher than 22 results in overflow)
If ( n < 3 ) ; values less than 3 are handled specially
   Return n < 0 ? "" : n = 0 ? 1 : n

i := 1 ; initialize the accumulator to 1

Loop % n - 1 >> 1 ; build the numerator by multiplying odd values between 2N and N+1
   i *= 1 + ( n - A_Index << 1 )

i <<= ( n - 2 >> 1 ) ; multiply the numerator by powers of 2 according to N

Loop % n - 3 >> 1 ; finish up by (integer) dividing by each of the non-cancelling factors
   i //= A_Index + 2

Return i
}
