#FileName="notes.txt"
Define argc=CountProgramParameters()
If OpenConsole()
  If argc=0
    If ReadFile(0,#FileName)
      While Eof(0)=0
        PrintN(ReadString(0))                    ; No new notes, so present the old
      Wend
      CloseFile(0)
    EndIf
  Else ; e.g. we have some arguments
    Define d$=FormatDate("%yyyy-%mm-%dd %hh:%ii:%ss",date())
    If OpenFile(0,#FileName)
      Define args$=""
      While argc
        args$+" "+ProgramParameter()             ; Read all arguments
        argc-1
      Wend
      FileSeek(0,Lof(0))                         ; Go to the end of this file
      WriteStringN(0,d$+#CRLF$+#TAB$+args$)      ; Append date & note
      CloseFile(0)
    EndIf
  EndIf
EndIf
