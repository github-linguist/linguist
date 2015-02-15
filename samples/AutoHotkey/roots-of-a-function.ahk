MsgBox % roots("poly", -0.99, 2, 0.1, 1.0e-5)
MsgBox % roots("poly", -1, 3, 0.1, 1.0e-5)

roots(f,x1,x2,step,tol) { ; search for roots in intervals of length "step", within tolerance "tol"
   x := x1, y := %f%(x), s := (y>0)-(y<0)
   Loop % ceil((x2-x1)/step) {
      x += step, y := %f%(x), t := (y>0)-(y<0)
      If (s=0 || s!=t)
         res .= root(f, x-step, x, tol) " [" ErrorLevel "]`n"
      s := t
   }
   Sort res, UN ; remove duplicate endpoints
   Return res
}

root(f,x1,x2,d) { ; find x in [x1,x2]: f(x)=0 within tolerance d, by bisection
   If (!y1 := %f%(x1))
      Return x1, ErrorLevel := "Exact"
   If (!y2 := %f%(x2))
      Return x2, ErrorLevel := "Exact"
   If (y1*y2>0)
      Return "", ErrorLevel := "Need different sign ends!"
   Loop {
      x := (x2+x1)/2, y := %f%(x)
      If (y = 0 || x2-x1 < d)
         Return x, ErrorLevel := y ? "Approximate" : "Exact"
      If ((y>0) = (y1>0))
         x1 := x, y1 := y
      Else
         x2 := x, y2 := y
   }
}

poly(x) {
   Return ((x-3)*x+2)*x
}
