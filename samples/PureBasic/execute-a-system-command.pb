ImportC "msvcrt.lib"
  system(str.p-ascii)
EndImport

If OpenConsole()
  system("dir & pause")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
