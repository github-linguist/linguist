MsgBox % noncontinuous("a,b,c,d,e", ",")
MsgBox % noncontinuous("1,2,3,4", ",")

noncontinuous(list, delimiter)
{
stringsplit, seq, list, %delimiter%
n := seq0                                            ; sequence length
Loop % x := (1<<n) - 1 {                                  ; try all 0-1 candidate sequences
   If !RegExMatch(b:=ToBin(A_Index,n),"^0*1*0*$") {  ; drop continuous subsequences
      Loop Parse, b
         t .= A_LoopField ? seq%A_Index% " " : ""         ; position -> number
		 t .= "`n"                                   ; new sequences in new lines
   }
}
return t
}

ToBin(n,W=16) {  ; LS W-bits of Binary representation of n
   Return W=1 ? n&1 : ToBin(n>>1,W-1) . n&1
}
