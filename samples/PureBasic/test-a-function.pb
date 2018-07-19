Macro DoubleQuote
  ; Needed for the Assert-Macro below
  "                             ; " second dlbquote to prevent Rosettas misshighlighting of following code. Remove comment before execution!
EndMacro
Macro Assert(TEST,MSG="")
  CompilerIf #PB_Compiler_Debugger
    If Not (TEST)
      If MSG<>"": Debug MSG: EndIf
      Temp$="Fail: "+DoubleQuote#TEST#DoubleQuote
      Debug Temp$+", Line="+Str(#PB_Compiler_Line)+" in "+#PB_Compiler_File
      CallDebugger
    EndIf
  CompilerEndIf
EndMacro

Procedure IsPalindrome(StringToTest.s)
  If StringToTest=ReverseString(StringToTest)
    ProcedureReturn 1
  Else
    ProcedureReturn 0
  EndIf
EndProcedure

text1$="racecar"
text2$="wisconsin"
Assert(IsPalindrome(text1$), "Catching this would be a fail")
Assert(IsPalindrome(text2$), "Catching this is correct")
