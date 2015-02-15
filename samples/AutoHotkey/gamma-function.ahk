/*
Here is the upper incomplete Gamma function. Omitting or setting
the second parameter to 0 we get the (complete) Gamma function.
The code is based on: "Computation of Special Functions" Zhang and Jin,
John Wiley and Sons, 1996
*/

SetFormat FloatFast, 0.9e

Loop 10
   MsgBox % GAMMA(A_Index/3) "`n" GAMMA(A_Index*10)

GAMMA(a,x=0) {  ; upper incomplete gamma: Integral(t**(a-1)*e**-t, t = x..inf)
   If (a > 171 || x < 0)
      Return 2.e308   ; overflow

   xam := x > 0 ? -x+a*ln(x) : 0
   If (xam > 700)
      Return 2.e308   ; overflow

   If (x > 1+a) {     ; no need for gamma(a)
      t0 := 0, k := 60
      Loop 60
          t0 := (k-a)/(1+k/(x+t0)), --k
      Return exp(xam) / (x+t0)
   }

   r := 1, ga := 1.0  ; compute ga = gamma(a) ...
   If (a = round(a))  ; if integer: factorial
      If (a > 0)
         Loop % a-1
            ga *= A_Index
      Else            ; negative integer
         ga := 1.7976931348623157e+308 ; Dmax
   Else {             ; not integer
      If (abs(a) > 1) {
         z := abs(a)
         m := floor(z)
         Loop %m%
             r *= (z-A_Index)
         z -= m
      }
      Else
         z := a

      gr := (((((((((((((((((((((((       0.14e-14
          *z - 0.54e-14)             *z - 0.206e-13)          *z + 0.51e-12)
          *z - 0.36968e-11)          *z + 0.77823e-11)        *z + 0.1043427e-9)
          *z - 0.11812746e-8)        *z + 0.50020075e-8)      *z + 0.6116095e-8)
          *z - 0.2056338417e-6)      *z + 0.1133027232e-5)    *z - 0.12504934821e-5)
          *z - 0.201348547807e-4)    *z + 0.1280502823882e-3) *z - 0.2152416741149e-3)
          *z - 0.11651675918591e-2)  *z + 0.7218943246663e-2) *z - 0.9621971527877e-2)
          *z - 0.421977345555443e-1) *z + 0.1665386113822915) *z - 0.420026350340952e-1)
          *z - 0.6558780715202538)   *z + 0.5772156649015329) *z + 1

      ga := 1.0/(gr*z) * r
      If (a < -1)
         ga := -3.1415926535897931/(a*ga*sin(3.1415926535897931*a))
   }

   If (x = 0)         ; complete gamma requested
      Return ga

   s := 1/a           ; here x <= 1+a
   r := s
   Loop 60 {
      r *= x/(a+A_Index)
      s += r
      If (abs(r/s) < 1.e-15)
         break
   }
   Return ga - exp(xam)*s
}

/*
The 10 results shown:
2.678938535e+000  1.354117939e+000  1.0               8.929795115e-001  9.027452930e-001
3.628800000e+005  1.216451004e+017  8.841761994e+030  2.039788208e+046  6.082818640e+062

1.000000000e+000  1.190639348e+000  1.504575489e+000  2.000000000e+000  2.778158479e+000
1.386831185e+080  1.711224524e+098  8.946182131e+116  1.650795516e+136  9.332621544e+155
*/
