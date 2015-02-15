; The following directives and commands speed up execution:
#NoEnv
SetBatchlines -1
ListLines Off
Process, Priority,, high

MsgBox % 2020 ": " IsSelfDescribing(2020) "`n" 1337 ": " IsSelfDescribing(1337) "`n" 1210 ": " IsSelfDescribing(1210)
Loop 100000000
   If IsSelfDescribing(A_Index)
      list .= A_Index "`n"
MsgBox % "Self-describing numbers < 100000000 :`n" . list

CountSubstring(fullstring, substring){
   StringReplace, junk, fullstring, %substring%, , UseErrorLevel
   return errorlevel
}

IsSelfDescribing(number){
   Loop Parse, number
      If Not CountSubString(number, A_Index-1) = A_LoopField
         return false
   return true
}
