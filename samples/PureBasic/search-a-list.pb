If OpenConsole()  ; Open a simple console to interact with user
  NewList Straws.s()
  Define Straw$, target$="TBA"
  Define found

  Restore haystack ; Read in all the straws of the haystack.
  Repeat
    Read.s Straw$
    If Straw$<>""
      AddElement(Straws())
      Straws()=UCase(Straw$)
      Continue
    Else
      Break
    EndIf
  ForEver

  While target$<>""
    Print(#CRLF$+"Enter word to search for (leave blank to quit) :"): target$=Input()
    ResetList(Straws()): found=#False
    While NextElement(Straws())
      If UCase(target$)=Straws()
        found=#True
        PrintN(target$+" found as index #"+Str(ListIndex(Straws())))
      EndIf
    Wend
    If Not found
      PrintN("Not found.")
    EndIf
  Wend
EndIf

DataSection
  haystack:
  Data.s "Zig","Zag","Zig","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo",""
EndDataSection
