Procedure isDirEmpty(path$)
  If Right(path$, 1) <> "\": path$ + "\": EndIf
  Protected dirID = ExamineDirectory(#PB_Any, path$, "*.*")
  Protected result

  If dirID
    result = 1
    While NextDirectoryEntry(dirID)
      If DirectoryEntryType(dirID) = #PB_DirectoryEntry_File Or (DirectoryEntryName(dirID) <> "." And DirectoryEntryName(dirID) <> "..")
        result = 0
        Break
      EndIf
    Wend
    FinishDirectory(dirID)
  EndIf
  ProcedureReturn result
EndProcedure

Define path$, result$

path$ = PathRequester("Choose a path", "C:\")
If path$
  If isDirEmpty(path$)
    result$ = " is empty."
  Else
    result$ = " is not empty."
  EndIf
  MessageRequester("Empty directory test", #DQUOTE$ + path$ + #DQUOTE$ + result$)
EndIf
