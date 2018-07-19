Procedure GRTISF(List File$(), Find$, Replace$)
  Protected Line$, Out$, OutFile$, i
  ForEach File$()
    fsize=FileSize(File$())
    If fsize<=0: Continue: EndIf
    If ReadFile(0, File$())
      i=0
      ;
      ; generate a temporary file in a safe way
      Repeat
        file$=GetTemporaryDirectory()+base$+"_"+Str(i)+".tmp"
        i+1
      Until FileSize(file$)=-1
      i=CreateFile(FileID, file$)
      If i
        ; Copy the infile to the outfile while replacing any needed text
        While Not Eof(0)
          Line$=ReadString(0)
          Out$=ReplaceString(Line$,Find$,Replace$)
          WriteString(1,Out$)
        Wend
        CloseFile(1)
      EndIf
      CloseFile(0)
      If i
        ; If we made a new file, copy it back.
        CopyFile(file$, File$())
        DeleteFile(file$)
      EndIf
    EndIf
  Next
EndProcedure
