If Not OpenConsole()
  End
ElseIf CountProgramParameters() <> 2
  PrintN("Usage: "+GetFilePart(ProgramFilename())+" InFile OutFile")
  End
EndIf

Define Infile$ =ProgramParameter(), Outfile$=ProgramParameter()
If ReadFile(0,Infile$)
  NewList Out$()
  Define line$, part$, new$, pos1, pos2
  While Not Eof(0)
    line$=ReadString(0): pos2=0
    Repeat
      pos1=FindString(line$,"<",pos2)
      pos2=FindString(line$,">",pos1)
      If pos1 And pos2
        part$=Mid(line$,pos1+1,pos2-pos1-1)
        If Mid(part$,1,1)="/"
          new$="<"+"/lang>" ; Line split to avoid problem forum coding
        ElseIf Mid(part$,1,5)="code "
          new$="<lang "+Mid(part$,6)+">"
        Else
          new$="<lang "+part$+">"
        EndIf
        line$=ReplaceString(line$,"<"+part$+">",new$)
      Else
        Break
      EndIf
    ForEver
    AddElement(Out$()): Out$()=line$
  Wend
  CloseFile(0)
  If CreateFile(1, Outfile$)
    ForEach Out$()
      WriteStringN(1,Out$())
    Next
    CloseFile(1)
  EndIf
EndIf
