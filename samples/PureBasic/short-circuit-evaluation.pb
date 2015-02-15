Procedure a(arg)
  PrintN("  # Called function a("+Str(arg)+")")
  ProcedureReturn arg
EndProcedure

Procedure b(arg)
  PrintN("  # Called function b("+Str(arg)+")")
  ProcedureReturn arg
EndProcedure

OpenConsole()
For a=#False To #True
  For b=#False To #True
    PrintN(#CRLF$+"Calculating: x = a("+Str(a)+") And b("+Str(b)+")")
    x= a(a) And b(b)
    PrintN("Calculating: x = a("+Str(a)+") Or b("+Str(b)+")")
    y= a(a) Or b(b)
  Next
Next
Input()
