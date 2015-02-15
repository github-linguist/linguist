If OpenConsole()
  Define i, txt$, choice
  Dim txts.s(4)
  EnableGraphicalConsole(1)  ;- Enable graphical mode in the console
  Repeat
    ClearConsole()
    Restore TheStrings       ; Set reads address
    For i=1 To 4
      Read.s  txt$
      txts(i)=txt$
      ConsoleLocate(3,i): Print(Str(i)+": "+txt$)
    Next
    ConsoleLocate(3,6): Print("Your choice? ")
    choice=Val(Input())
  Until choice>=1 And choice<=4
  ClearConsole()
  ConsoleLocate(3,2): Print("You chose: "+txts(choice))
  ;
  ;-Now, wait for the user before ending to allow a nice presentation
  ConsoleLocate(3,5): Print("Press ENTER to quit"): Input()
EndIf
End

DataSection
  TheStrings:
  Data.s  "fee fie", "huff And puff", "mirror mirror", "tick tock"
EndDataSection
