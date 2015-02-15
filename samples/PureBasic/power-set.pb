If OpenConsole()
  Define argc=CountProgramParameters()
  If argc>=(SizeOf(Integer)*8) Or argc<1
    PrintN("Set out of range.")
    End 1
  Else
    Define i, j, text$
    Define.q bset=1<<argc
    Print("{")
    For i=0 To bset-1   ; check all binary combinations
      If Not i: text$=  "{"
      Else    : text$=", {"
      EndIf
      k=0
      For j=0 To argc-1  ; step through each bit
        If i&(1<<j)
          If k: text$+", ": EndIf         ; pad the output
          text$+ProgramParameter(j): k+1  ; append each matching bit
        EndIf
      Next j
      Print(text$+"}")
    Next i
    PrintN("}")
  EndIf
EndIf
