MsgBox % CocktailSort("")
MsgBox % CocktailSort("xxx")
MsgBox % CocktailSort("3,2,1")
MsgBox % CocktailSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")

CocktailSort(var) {                      ; SORT COMMA SEPARATED LIST
   StringSplit array, var, `,            ; make array
   i0 := 1, i1 := array0                 ; start, end

   Loop {                                ; break when sorted
     Changed =
     Loop % i1-- -i0 {                   ; last entry will be in place
       j := i0+A_Index, i := j-1
       If (array%j% < array%i%)          ; swap?
         t := array%i%, array%i% := array%j%, array%j% := t
        ,Changed = 1                     ; change has happened
     }
     IfEqual Changed,, Break

     Loop % i1-i0++ {                    ; first entry will be in place
       i := i1-A_Index, j := i+1
       If (array%j% < array%i%)          ; swap?
         t := array%i%, array%i% := array%j%, array%j% := t
        ,Changed = 1                     ; change has happened
     }
     IfEqual Changed,, Break
   }

   Loop % array0                         ; construct string from sorted array
     sorted .= "," . array%A_Index%
   Return SubStr(sorted,2)               ; drop leading comma
}
