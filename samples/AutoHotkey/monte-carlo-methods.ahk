MsgBox % MontePi(10000)   ; 3.154400
MsgBox % MontePi(100000)  ; 3.142040
MsgBox % MontePi(1000000) ; 3.142096

MontePi(n) {
   Loop %n% {
      Random x, -1, 1.0
      Random y, -1, 1.0
      p += x*x+y*y < 1
   }
   Return 4*p/n
}
