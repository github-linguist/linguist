a = 1,a,--             ; elements separated by commas
StringSplit a, a, `,   ; a0 = #elements, a1,a2,... = elements of the set

t = {
Loop % (1<<a0) {       ; generate all 0-1 sequences
   x := A_Index-1
   Loop % a0
      t .= (x>>A_Index-1) & 1 ? a%A_Index% "," : ""
   t .= "}`n{"         ; new subsets in new lines
}
MsgBox % RegExReplace(SubStr(t,1,StrLen(t)-1),",}","}")
