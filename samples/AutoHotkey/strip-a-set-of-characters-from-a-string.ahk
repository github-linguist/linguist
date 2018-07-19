MsgBox % stripchars("She was a soul stripper. She took my heart!","aei")

StripChars(string, charsToStrip){
   Loop Parse, charsToStrip
      StringReplace, string, string, % A_LoopField, , All
   return string
}
