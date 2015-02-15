CompilerIf #PB_Compiler_OS<>#PB_OS_Windows
  CompilerError "This code is Windows only"
CompilerEndIf

Global Quit, i, T0=ElapsedMilliseconds(), T1

Procedure CtrlC()
  T1=ElapsedMilliseconds()
  Quit=1
  While i: Delay(1): Wend
EndProcedure

If OpenConsole()
  SetConsoleCtrlHandler_(@CtrlC(),#True)
  While Not Quit
    PrintN(Str(i))
    i+1
    Delay(500)
  Wend
  PrintN("Program has run for "+StrF((T1-T0)/1000,3)+" seconds.")
  Print ("Press ENTER to exit."):Input(): i=0
EndIf
