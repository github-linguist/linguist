result = ReadFile(#PB_Any, "input.txt")
If result>0 : Debug "this local file exists"
  Else : Debug "result=" +Str(result) +" so this local file is missing"
EndIf

result = ReadFile(#PB_Any, "/input.txt")
If result>0 : Debug "this root file exists"
  Else : Debug "result=" +Str(result) +" so this root file is missing"
EndIf

result = ExamineDirectory(#PB_Any,"docs","")
If result>0 : Debug "this local directory exists"
  Else : Debug "result=" +Str(result) +" so this local directory is missing"
EndIf

result = ExamineDirectory(#PB_Any,"/docs","")
If result>0 : Debug "this root directory exists"
  Else : Debug "result=" +Str(result) +" so this root directory is missing"
EndIf
