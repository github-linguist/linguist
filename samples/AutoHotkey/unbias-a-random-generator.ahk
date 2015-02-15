Biased(){
   Random, q, 0, 4
   return q=4
}
Unbiased(){
   Loop
      If ((a := Biased()) != biased())
          return a
}
Loop 1000
   t .= biased(), t2 .= unbiased()
StringReplace, junk, t2, 1, , UseErrorLevel
MsgBox % "Unbiased probability of a 1 occurring: " Errorlevel/1000
StringReplace, junk, t, 1, , UseErrorLevel
MsgBox % "biased probability of a 1 occurring: " Errorlevel/1000
