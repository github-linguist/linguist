MsgBox % ShellSort("")
MsgBox % ShellSort("xxx")
MsgBox % ShellSort("3,2,1")
MsgBox % ShellSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")
MsgBox % ShellSort("12,11,10,9,8,4,5,6,7,3,2,1,10,13,14,15,19,17,18,16,20,10")

ShellSort(var) {                         ; SORT COMMA SEPARATED LIST
   StringSplit a, var, `,                ; make array (length = a0)
   inc := a0
   While inc:=round(inc/2.2)             ; geometric gap sequence
      Loop % a0-inc {                    ; insertion sort:
         i := A_Index+inc, t := a%i%, j := i, k := j-inc
         While j > inc && a%k% > t
            a%j% := a%k%, j := k, k -= inc
         a%j% := t
      }
   Loop % a0                             ; construct string from sorted array
      s .= "," . a%A_Index%
   Return SubStr(s,2)                    ; drop leading comma
}
