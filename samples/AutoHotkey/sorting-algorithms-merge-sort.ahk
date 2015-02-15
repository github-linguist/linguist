MsgBox % MSort("")
MsgBox % MSort("xxx")
MsgBox % MSort("3,2,1")
MsgBox % MSort("dog,000000,cat,pile,abcde,1,zz,xx,z")

MSort(x) {                                                  ; Merge-sort of a comma separated list
   If (2 > L:=Len(x))
       Return x                                             ; empty or single item lists are sorted
   StringGetPos p, x, `,, % "L" L//2                        ; Find middle comma
   Return Merge(MSort(SubStr(x,1,p)), MSort(SubStr(x,p+2))) ; Split, Sort, Merge
}

Len(list) {
   StringReplace t, list,`,,,UseErrorLevel                  ; #commas -> ErrorLevel
   Return list="" ? 0 : ErrorLevel+1
}

Item(list,ByRef p) {                                        ; item at position p, p <- next position
   Return (p := InStr(list,",",0,i:=p+1)) ? SubStr(list,i,p-i) : SubStr(list,i)
}

Merge(list0,list1) {                                        ; Merge 2 sorted lists
   IfEqual list0,, Return list1
   IfEqual list1,, Return list0
   i0 := Item(list0,p0:=0)
   i1 := Item(list1,p1:=0)
   Loop  {
      i := i0>i1
      list .= "," i%i%                                      ; output smaller
      If (p%i%)
         i%i% := Item(list%i%,p%i%)                         ; get next item from processed list
      Else {
         i ^= 1                                             ; list is exhausted: attach rest of other
         Return SubStr(list "," i%i% (p%i% ? "," SubStr(list%i%,p%i%+1) : ""), 2)
      }
   }
}
