MsgBox % MillerRabin(999983,10) ; 1
MsgBox % MillerRabin(999809,10) ; 1
MsgBox % MillerRabin(999727,10) ; 1
MsgBox % MillerRabin(52633,10)  ; 0
MsgBox % MillerRabin(60787,10)  ; 0
MsgBox % MillerRabin(999999,10) ; 0
MsgBox % MillerRabin(999995,10) ; 0
MsgBox % MillerRabin(999991,10) ; 0

MillerRabin(n,k) { ; 0: composite, 1: probable prime (n < 2**31)
   d := n-1, s := 0
   While !(d&1)
      d>>=1, s++

   Loop %k% {
      Random a, 2, n-2 ; if n < 4,759,123,141, it is enough to test a = 2, 7, and 61.
      x := PowMod(a,d,n)
      If (x=1 || x=n-1)
         Continue
      Cont := 0
      Loop % s-1 {
         x := PowMod(x,2,n)
         If (x = 1)
            Return 0
         If (x = n-1) {
            Cont = 1
            Break
         }
      }
      IfEqual Cont,1, Continue
      Return 0
   }
   Return 1
}

PowMod(x,n,m) { ; x**n mod m
   y := 1, i := n, z := x
   While i>0
      y := i&1 ? mod(y*z,m) : y, z := mod(z*z,m), i >>= 1
   Return y
}
