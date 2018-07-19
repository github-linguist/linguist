MsgBox % shuffle("1,2,3,4,5,6,7,8,9")
MsgBox % shuffle("1,2,3,4,5,6,7,8,9")

shuffle(list) {                          ; shuffle comma separated list, converted to array
   StringSplit a, list, `,               ; make array (length = a0)
   Loop % a0-1 {
      Random i, A_Index, a0              ; swap item 1,2... with a random item to the right of it
      t := a%i%, a%i% := a%A_Index%, a%A_Index% := t
   }
   Loop % a0                             ; construct string from sorted array
      s .= "," . a%A_Index%
   Return SubStr(s,2)                    ; drop leading comma
}
