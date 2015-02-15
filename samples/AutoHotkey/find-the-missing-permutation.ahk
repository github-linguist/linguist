IncompleteList := "ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB"

CompleteList := Perm( "ABCD" )
Missing := ""

Loop, Parse, CompleteList, `n, `r
  If !InStr( IncompleteList , A_LoopField )
    Missing .= "`n" A_LoopField

MsgBox Missing Permutation(s):%Missing%

;-------------------------------------------------

; Shortened version of [VxE]'s permutation function
; http://www.autohotkey.com/forum/post-322251.html#322251
Perm( s , dL="" , t="" , p="") {
   StringSplit, m, s, % d := SubStr(dL,1,1) , %t%
   IfEqual, m0, 1, return m1 d p
   Loop %m0%
   {
      r := m1
      Loop % m0-2
         x := A_Index + 1, r .= d m%x%
      L .= Perm(r, d, t, m%m0% d p)"`n" , mx := m1
      Loop % m0-1
         x := A_Index + 1, m%A_Index% := m%x%
      m%m0% := mx
   }
   return substr(L, 1, -1)
}
