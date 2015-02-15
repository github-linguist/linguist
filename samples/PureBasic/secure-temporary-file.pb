Procedure.s TempFile()
  Protected a, Result$

  For a = 0 To 9999
    Result$ = GetTemporaryDirectory() + StringField(GetFilePart(ProgramFilename()),1,".")
    Result$ + "_" + Str(ElapsedMilliseconds()) + "_(" + RSet(Str(a),4,"0") + ").tmp"
    If FileSize(Result$) = -1                                      ;  -1 = File not found
      ProcedureReturn Result$
    EndIf
  Next

  ProcedureReturn ""
EndProcedure


Define File, File$

File$ = TempFile()
If File$ <> ""
  File = CreateFile(#PB_Any, File$)
  If File <> 0
    WriteString(File, "Some temporary data here...")
    CloseFile(File)
  EndIf
EndIf
