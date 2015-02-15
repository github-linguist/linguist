#coupleCount = 10

DataSection
  ;guys
  Data.s "abe: abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay"
  Data.s "bob: cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay"
  Data.s "col: hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan"
  Data.s "dan: ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi"
  Data.s "ed: jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay"
  Data.s "fred: bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay"
  Data.s "gav: gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay"
  Data.s "hal: abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee"
  Data.s "ian: hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve"
  Data.s "jon: abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope"
  ;gals
  Data.s "abi: bob, fred, jon, gav, ian, abe, dan, ed, col, hal"
  Data.s "bea: bob, abe, col, fred, gav, dan, ian, ed, jon, hal"
  Data.s "cath: fred, bob, ed, gav, hal, col, ian, abe, dan, jon"
  Data.s "dee: fred, jon, col, abe, ian, hal, gav, dan, bob, ed"
  Data.s "eve: jon, hal, fred, dan, abe, gav, col, ed, ian, bob"
  Data.s "fay: bob, abe, ed, ian, jon, dan, fred, gav, col, hal"
  Data.s "gay: jon, gav, hal, fred, bob, abe, col, ed, dan, ian"
  Data.s "hope: gav, jon, bob, abe, ian, dan, hal, ed, col, fred"
  Data.s "ivy: ian, col, hal, gav, fred, bob, abe, ed, jon, dan"
  Data.s "jan: ed, hal, gav, abe, bob, jon, col, ian, fred, dan"
EndDataSection

Structure message
  source.s ;person that message is from
  dest.s   ;person that message is for
  action.s ;{'P', 'A', 'D', 'B'} for proposal, accept, decline, break-up
EndStructure

Structure person
  name.s
  isEngagedTo.s
  List prefs.s()
EndStructure

Global NewList messages.message()

Procedure setupPersons(List persons.person(), count)
  Protected i, j, start, pref$
  For i = 1 To count
    Read.s pref$
    pref$ = LCase(pref$)
    start = FindString(pref$, ":", 1)
    AddElement(persons())
    persons()\name = Left(pref$, start - 1)
    pref$ = Trim(Right(pref$, Len(pref$) - start))
    For j = 1 To count
      AddElement(persons()\prefs())
      persons()\prefs() = Trim(StringField(pref$, j, ","))
    Next
  Next
EndProcedure

Procedure sendMessage(source.s, dest.s, action.s)
  LastElement(messages())
  AddElement(messages())
  With messages()
    \source = source
    \dest = dest
    \action = action
  EndWith
  ResetList(messages())
EndProcedure

Procedure selectPerson(name.s, List persons.person())
  ForEach persons()
    If persons()\name = name
      Break
    EndIf
  Next
EndProcedure

Procedure rankPerson(name.s, List prefs.s())
  ForEach prefs()
    If prefs() = name
      ProcedureReturn #coupleCount - ListIndex(prefs()) ;higher is better
    EndIf
  Next
  ProcedureReturn -1 ;no rank, shouldn't occur
EndProcedure

Procedure stabilityCheck(List guys.person(), List gals.person())
  Protected isStable = #True
  ForEach guys()
    rankPerson(guys()\isEngagedTo, guys()\prefs())
    While PreviousElement(guys()\prefs())
      selectPerson(guys()\prefs(), gals())
      If rankPerson(guys()\name, gals()\prefs()) > rankPerson(gals()\isEngagedTo, gals()\prefs())
        Print("  " + gals()\name + " loves " + guys()\name + " more than " + gals()\isEngagedTo + ",")
        PrintN(" And " + guys()\name + " loves " + gals()\name + " more than " + guys()\isEngagedTo + ".")
        isStable = #False
      EndIf
    Wend
  Next
  If isStable
    PrintN(#CRLF$ + "Marriage stability check PASSED.")
  Else
    PrintN(#CRLF$ + "Marriage stability check FAILED.")
  EndIf
EndProcedure

NewList guys.person()
NewList gals.person()
setupPersons(guys(), #coupleCount)
setupPersons(gals(), #coupleCount)

;make initial round of proposals
ForEach guys()
  FirstElement(guys()\prefs())
  sendMessage(guys()\name, guys()\prefs(), "P")
Next

;dispatch messages
Define source.s, dest.s, action.s
ForEach messages()
  source = messages()\source
  dest = messages()\dest
  action = messages()\action

  DeleteElement(messages())
  Select action
    Case "P" ;propose ;only message received by gals
      selectPerson(dest, gals())
      selectPerson(source, guys())
      If rankPerson(guys()\name, gals()\prefs()) < rankPerson(gals()\isEngagedTo, gals()\prefs())
        sendMessage(dest, source, "D") ;decline proposal
      ElseIf rankPerson(guys()\name, gals()\prefs()) > rankPerson(gals()\isEngagedTo, gals()\prefs())
        If gals()\isEngagedTo
          sendMessage(dest, gals()\isEngagedTo, "B")  ;break-up engagement
        EndIf
        gals()\isEngagedTo = source
        sendMessage(dest, source, "A") ;accept proposal
      EndIf
    Case "A", "D", "B" ;messages received by guys
      selectPerson(dest, guys())
      If action = "A" ;proposal accepted
        guys()\isEngagedTo = source
      Else
        If action = "B" ;broke-up
          guys()\isEngagedTo = ""
        EndIf
        NextElement(guys()\prefs())
        sendMessage(dest, guys()\prefs(),"P") ;propose to next pref
      EndIf
  EndSelect
Next

If OpenConsole()
  PrintN("Marriages:")
  ForEach guys()
    PrintN("  " + guys()\name + " And " + guys()\isEngagedTo + ".")
  Next
  stabilityCheck(guys(), gals())

  Define *person_1.person, *person_2.person
  PrintN(#CRLF$ + "Introducing an error by swapping partners of abi and bea.")
  selectPerson("abi", gals()): *person_1 = @gals()
  selectPerson("bea", gals()): *person_2 = @gals()
  Swap *person_1\isEngagedTo, *person_2\isEngagedTo
  selectPerson(*person_1\isEngagedTo, guys()): *person_1 = @guys()
  selectPerson(*person_1\isEngagedTo, guys()): *person_2 = @guys()
  Swap *person_1\isEngagedTo, *person_2\isEngagedTo
  PrintN("  " + *person_1\name + " is now with " + *person_1\isEngagedTo + ".")
  PrintN("  " + *person_2\name + " is now with " + *person_2\isEngagedTo + ".")
  PrintN("")
  stabilityCheck(guys(), gals())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
