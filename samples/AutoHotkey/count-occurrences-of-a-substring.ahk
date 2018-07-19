MsgBox % countSubstring("the three truths","th") ; 3
MsgBox % countSubstring("ababababab","abab")     ; 2

CountSubstring(fullstring, substring){
   StringReplace, junk, fullstring, %substring%, , UseErrorLevel
   return errorlevel
}
