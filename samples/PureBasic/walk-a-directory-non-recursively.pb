Procedure walkDirectory(directory.s = "", pattern.s = "")
  Protected directoryID

  directoryID = ExamineDirectory(#PB_Any,directory,pattern)
  If directoryID
    While NextDirectoryEntry(directoryID)
      PrintN(DirectoryEntryName(directoryID))
    Wend
    FinishDirectory(directoryID)
  EndIf
EndProcedure

If OpenConsole()
  walkDirectory()

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
