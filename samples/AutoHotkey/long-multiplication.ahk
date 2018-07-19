MsgBox % x := mul(256,256)
MsgBox % x := mul(x,x)
MsgBox % x := mul(x,x) ; 18446744073709551616
MsgBox % x := mul(x,x) ; 340282366920938463463374607431768211456

mul(b,c) { ; <- b*c
   VarSetCapacity(a, n:=StrLen(b)+StrLen(c), 48), NumPut(0,a,n,"char")
   Loop % StrLen(c) {
      i := StrLen(c)+1-A_Index, cy := 0
      Loop % StrLen(b) {
         j := StrLen(b)+1-A_Index,
         t := SubStr(a,i+j,1) + SubStr(b,j,1) * SubStr(c,i,1) + cy
         cy := t // 10
         NumPut(mod(t,10)+48,a,i+j-1,"char")
      }
      NumPut(cy+48,a,i+j-2,"char")
   }
   Return cy ? a : SubStr(a,2)
}
