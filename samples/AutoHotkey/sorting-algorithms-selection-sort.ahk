MsgBox % SelecSort("")
MsgBox % SelecSort("xxx")
MsgBox % SelecSort("3,2,1")
MsgBox % SelecSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")

SelecSort(var) {                         ; SORT COMMA SEPARATED LIST
   StringSplit a, var, `,                ; make array, size = a0

   Loop % a0-1 {
      i := A_Index, mn := a%i%, j := m := i
      Loop % a0-i {                      ; find minimum
          j++
          If (a%j% < mn)
             mn := a%j%, m := j
      }
      t := a%i%, a%i% := a%m%, a%m% := t ; swap first with minimum
   }
   Loop % a0                             ; construct string from sorted array
      sorted .= "," . a%A_Index%
   Return SubStr(sorted,2)               ; drop leading comma
}
