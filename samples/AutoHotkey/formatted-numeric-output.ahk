MsgBox % pad(7.25,7)  ; 0007.25
MsgBox % pad(-7.25,7) ; -007.25

pad(x,len) { ; pad with 0's from left to len chars
   IfLess x,0, Return "-" pad(SubStr(x,2),len-1)
   VarSetCapacity(p,len,Asc("0"))
   Return SubStr(p x,1-len)
}
