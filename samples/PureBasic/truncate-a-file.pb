Procedure SetFileSize(File$, length.q)
  Protected fh, pos, i
  If FileSize(File$) < length
    Debug "File to small, is a directory or does not exist."
    ProcedureReturn #False
  Else
    fh = OpenFile(#PB_Any, File$)
    FileSeek(fh, length)
    TruncateFile(fh)
    CloseFile(fh)
  EndIf
  ProcedureReturn #True
EndProcedure
