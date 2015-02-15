MsgBox % MFact(27)  ;-1: 27 is not prime
MsgBox % MFact(2)   ; 0
MsgBox % MFact(3)   ; 0
MsgBox % MFact(5)   ; 0
MsgBox % MFact(7)   ; 0
MsgBox % MFact(11)  ; 23
MsgBox % MFact(13)  ; 0
MsgBox % MFact(17)  ; 0
MsgBox % MFact(19)  ; 0
MsgBox % MFact(23)  ; 47
MsgBox % MFact(29)  ; 233
MsgBox % MFact(31)  ; 0
MsgBox % MFact(37)  ; 223
MsgBox % MFact(41)  ; 13367
MsgBox % MFact(43)  ; 431
MsgBox % MFact(47)  ; 2351
MsgBox % MFact(53)  ; 6361
MsgBox % MFact(929) ; 13007

MFact(p) { ; blank if 2**p-1 can be prime, otherwise a prime divisor < 2**32
   If !IsPrime32(p)
      Return -1                      ; Error (p must be prime)
   Loop % 2.0**(p<64 ? p/2-1 : 31)/p ; test prime divisors < 2**32, up to sqrt(2**p-1)
      If (((q:=2*p*A_Index+1)&7 = 1 || q&7 = 7) && IsPrime32(q) && PowMod(2,p,q)=1)
         Return q
   Return 0
}

IsPrime32(n) { ; n < 2**32
   If n in 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
      Return 1
   If (!(n&1)||!mod(n,3)||!mod(n,5)||!mod(n,7)||!mod(n,11)||!mod(n,13)||!mod(n,17)||!mod(n,19))
      Return 0
   n1 := d := n-1, s := 0
   While !(d&1)
      d>>=1, s++
   Loop 3 {
      x := PowMod( A_Index=1 ? 2 : A_Index=2 ? 7 : 61, d, n)
      If (x=1 || x=n1)
         Continue
      Loop % s-1
         If (1 = x:=PowMod(x,2,n))
            Return 0
         Else If (x = n1)
            Break
      IfLess x,%n1%, Return 0
   }
   Return 1
}

PowMod(x,n,m) { ; x**n mod m
   y := 1, i := n, z := x
   While i>0
      y := i&1 ? mod(y*z,m) : y, z := mod(z*z,m), i >>= 1
   Return y
}
