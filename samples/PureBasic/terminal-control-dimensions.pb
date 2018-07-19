Macro ConsoleHandle()
  GetStdHandle_( #STD_OUTPUT_HANDLE )
EndMacro

Procedure ConsoleWidth()
  Protected CBI.CONSOLE_SCREEN_BUFFER_INFO
  Protected hConsole = ConsoleHandle()
  GetConsoleScreenBufferInfo_( hConsole, @CBI )
  ProcedureReturn CBI\srWindow\right - CBI\srWindow\left + 1
EndProcedure

Procedure ConsoleHeight()
  Protected CBI.CONSOLE_SCREEN_BUFFER_INFO
  Protected hConsole = ConsoleHandle()
  GetConsoleScreenBufferInfo_( hConsole, @CBI )
  ProcedureReturn CBI\srWindow\bottom - CBI\srWindow\top + 1
EndProcedure

If OpenConsole()
  x$=Str(ConsoleWidth())
  y$=Str(ConsoleHeight())
  PrintN("This window is "+x$+"x"+y$+ " chars.")
  ;
  Print(#CRLF$+"Press ENTER to exit"):Input()
EndIf
