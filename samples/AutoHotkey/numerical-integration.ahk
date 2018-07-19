MsgBox % Rect("fun", 0, 1, 10,-1) ; 0.45 left
MsgBox % Rect("fun", 0, 1, 10)    ; 0.50 mid
MsgBox % Rect("fun", 0, 1, 10, 1) ; 0.55 right
MsgBox % Trapez("fun", 0, 1, 10)  ; 0.50
MsgBox % Simpson("fun", 0, 1, 10) ; 0.50

Rect(f,a,b,n,side=0) { ; side: -1=left, 0=midpoint, 1=right
   h := (b - a) / n
   sum := 0, a += (side-1)*h/2
   Loop %n%
      sum += %f%(a + h*A_Index)
   Return h*sum
}

Trapez(f,a,b,n) {
   h := (b - a) / n
   sum := 0
   Loop % n-1
      sum += %f%(a + h*A_Index)
   Return h/2 * (%f%(a) + %f%(b) + 2*sum)
}

Simpson(f,a,b,n) {
   h := (b - a) / n
   sum1 := sum2 := 0, ah := a - h/2
   Loop %n%
      sum1 += %f%(ah + h*A_Index)
   Loop % n-1
      sum2 += %f%(a + h*A_Index)
   Return h/6 * (%f%(a) + %f%(b) + 4*sum1 + 2*sum2)
}

fun(x) { ; linear test function
   Return x
}
