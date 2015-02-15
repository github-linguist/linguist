MsgBox % GnomeSort("")
MsgBox % GnomeSort("xxx")
MsgBox % GnomeSort("3,2,1")
MsgBox % GnomeSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")

GnomeSort(var) {                         ; SORT COMMA SEPARATED LIST
   StringSplit a, var, `,                ; make array, size = a0
   i := 2, j := 3
   While i <= a0 {                       ; stop when sorted
      u := i-1
      If (a%u% < a%i%)                   ; search for pairs to swap
         i := j, j := j+1
      Else {                             ; swap
         t := a%u%, a%u% := a%i%, a%i% := t
         If (--i = 1)                    ; restart search
            i := j, j++
      }
   }
   Loop % a0                             ; construct string from sorted array
      sorted .= "," . a%A_Index%
   Return SubStr(sorted,2)               ; drop leading comma
}
