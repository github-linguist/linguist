Loop 4
   MsgBox % Carpet(A_Index)

Carpet(n) {
   Loop % 3**n {
      x := A_Index-1
      Loop % 3**n
         t .= Dot(x,A_Index-1)
      t .= "`n"
   }
   Return t
}

Dot(x,y) {
   While x>0 && y>0
      If (mod(x,3)=1 && mod(y,3)=1)
         Return " "
      Else x //= 3, y //= 3
   Return "."
}
