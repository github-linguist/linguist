MsgBox % PermSort("")
MsgBox % PermSort("xxx")
MsgBox % PermSort("3,2,1")
MsgBox % PermSort("dog,000000,xx,cat,pile,abcde,1,cat")

PermSort(var) {                          ; SORT COMMA SEPARATED LIST
   Local i, sorted
   StringSplit a, var, `,                ; make array, size = a0

   v0 := a0                              ; auxiliary array for permutations
   Loop %v0%
      v%A_Index% := A_Index

   While unSorted("a","v")               ; until sorted
      NextPerm("v")                      ; try new permutations

   Loop % a0                             ; construct string from sorted array
      i := v%A_Index%, sorted .= "," . a%i%
   Return SubStr(sorted,2)               ; drop leading comma
}

unSorted(a,v) {
   Loop % %a%0-1 {
      i := %v%%A_Index%, j := A_Index+1, j := %v%%j%
      If (%a%%i% > %a%%j%)
         Return 1
   }
}

NextPerm(v) { ; the lexicographically next LARGER permutation of v1..v%v0%
   Local i, i1, j, t
   i := %v%0, i1 := i-1
   While %v%%i1% >= %v%%i% {
      --i, --i1
      IfLess i1,1, Return 1 ; Signal the end
   }
   j := %v%0
   While %v%%j% <= %v%%i1%
      --j
   t := %v%%i1%, %v%%i1% := %v%%j%, %v%%j% := t,  j := %v%0
   While i < j
      t := %v%%i%, %v%%i% := %v%%j%, %v%%j% := t, ++i, --j
}
