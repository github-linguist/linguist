MsgBox % InsertionSort("")
MsgBox % InsertionSort("xxx")
MsgBox % InsertionSort("3,2,1")
MsgBox % InsertionSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")

InsertionSort(var) {                     ; SORT COMMA SEPARATED LIST
   StringSplit a, var, `,                ; make array, size = a0
   Loop % a0-1 {
      i := A_Index+1, v := a%i%, j := i-1
      While j>0 and a%j%>v
         u := j+1, a%u% := a%j%, j--
      u := j+1, a%u% := v
   }
   Loop % a0                             ; construct string from sorted array
      sorted .= "," . a%A_Index%
   Return SubStr(sorted,2)               ; drop leading comma
}
