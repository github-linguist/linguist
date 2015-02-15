MsgBox % quadratic(u,v, 1,-3,2) ", " u ", " v
MsgBox % quadratic(u,v, 1,3,2) ", " u ", " v
MsgBox % quadratic(u,v, -2,4,-2) ", " u ", " v
MsgBox % quadratic(u,v, 1,0,1) ", " u ", " v
SetFormat FloatFast, 0.15e
MsgBox % quadratic(u,v, 1,-1.0e8,1) ", " u ", " v

quadratic(ByRef x1, ByRef x2, a,b,c) { ; -> #real roots {x1,x2} of ax²+bx+c
   If (a = 0)
      Return -1 ; ERROR: not quadratic
   d := b*b - 4*a*c
   If (d < 0) {
      x1 := x2 := ""
      Return 0
   }
   If (d = 0) {
      x1 := x2 := -b/2/a
      Return 1
   }
   x1 := (-b - (b<0 ? -sqrt(d) : sqrt(d)))/2/a
   x2 := c/a/x1
   Return 2
}
