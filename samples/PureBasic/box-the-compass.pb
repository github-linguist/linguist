DataSection
  Data.s "N", "north", "E", "east", "W", "west", "S", "south", "b", " by "   ;abbreviations, expansions
  Data.s "N NbE N-NE NEbN NE NEbE E-NE EbN E EbS E-SE SEbE SE SEbS S-SE SbE" ;dirs
  Data.s "S SbW S-SW SWbS SW SWbW W-SW WbS W WbN W-NW NWbW NW NWbN N-NW NbW"
EndDataSection

;initialize data
NewMap dirSubst.s()
Define i, abbr.s, expansion.s
For i = 1 To 5
  Read.s abbr
  Read.s expansion
  dirSubst(abbr) = expansion
Next

Dim dirs.s(32)
Define j, s.s
For j = 0 To 1
  Read.s s.s
  For i = 0 To 15
    abbr.s = StringField(s, i + 1, " ")
    dirs(j * 16 + i) = abbr
  Next
Next

;expand abbreviated compass point and capitalize
Procedure.s abbr2compassPoint(abbr.s)
  Shared dirSubst()
  Protected i, compassPoint.s, key.s

  For i = 1 To Len(abbr)
    key.s = Mid(abbr, i, 1)
    If FindMapElement(dirSubst(), key)
      compassPoint + dirSubst(key)
    Else
      compassPoint + key
    EndIf
  Next
  ProcedureReturn UCase(Left(compassPoint, 1)) + Mid(compassPoint, 2)
EndProcedure

Procedure.s angle2compass(angle.f)
  Shared dirs()
  Static segment.f = 360.0 / 32 ;width of each compass segment
  Protected dir

  ;work out which segment contains the compass angle
  dir = Int((Mod(angle, 360) / segment) + 0.5)

  ;convert to a named direction
  ProcedureReturn abbr2compassPoint(dirs(dir))
EndProcedure

;box the compass
If OpenConsole()

  Define i, heading.f, index
  For i = 0 To 32
    heading = i * 11.25
    If i % 3 = 1
      heading + 5.62
    EndIf
    If i % 3 = 2
      heading - 5.62
    EndIf
    index = i % 32 + 1

    PrintN(RSet(Str(index), 2) + " " + LSet(angle2compass(heading), 18) + RSet(StrF(heading, 2), 7))
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
