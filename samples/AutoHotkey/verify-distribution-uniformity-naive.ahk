MsgBox, % DistCheck("dice7",10000,3)

DistCheck(function, repetitions, delta)
{  Loop, % 7 ; initialize array
   {  bucket%A_Index% := 0
   }

   Loop, % repetitions ; populate buckets
   {  v := %function%()
      bucket%v% += 1
   }

   lbnd := round((repetitions/7)*(100-delta)/100)
   ubnd := round((repetitions/7)*(100+delta)/100)
   text := "Distribution check:`n`nTotal elements = " repetitions
         . "`n`nMargin = " delta "% --> Lbound = " lbnd ", Ubound = " ubnd "`n"
   Loop, % 7
   {  text := text "`nBucket " A_Index " contains " bucket%A_Index% " elements."
      If bucket%A_Index% not between %lbnd% and %ubnd%
         text := text " Skewed."
   }
   Return, text
}
