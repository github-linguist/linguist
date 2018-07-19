Item  = Panacea,Ichor,Gold
Value = 3000,1800,2500
Weight= 3,2,20                         ; *10
Volume= 25,15,2                        ; *1000

StringSplit I, Item,  `,               ; Put input in arrays
StringSplit W, Weight,`,
StringSplit $, Value, `,
StringSplit V, Volume,`,

SetFormat Float, 0.3
W := 250, V := 250, sW:=.1, sV:=.001   ; limits for the total, scale factors
p := -1, Wp := -W1, Vp := -V1          ; initial values
While (Wp+=W1) <= W && (Vp+=V1) <= V {
   p++, Wi := Wp-W2, Vi := Vp-V2, i := -1
   While (Wi+=W2) <= W && (Vi+=V2) <= V {
      i++, Wg := Wi-W3, Vg := Vi-V3, g := -1
      While (Wg+=W3) <= W && (Vg+=V3) <= V
         If ($ <= Val := p*$1 + i*$2 + ++g*$3)
             t := ($=Val ? t "`n    " : "    ")
           . p "`t   " i "`t   " g "`t  " Wg*sW "`t   " Vg*sV
           , $ := Val
   }
}
MsgBox Value = %$%`n`nPanacea`tIchor`tGold`tWeight`tVolume`n%t%
