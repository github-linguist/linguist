;Declare Procedures
Declare.s MonthInText()
Declare.s DayInText()

;Output the requested strings
Debug FormatDate("%yyyy-%mm-%dd", Date())
Debug DayInText() + ", " + MonthInText() + FormatDate(" %dd, %yyyy", Date())

;Used procedures
Procedure.s DayInText()
  Protected d$
  Select DayOfWeek(Date())
    Case 1: d$="Monday"
    Case 2: d$="Tuesday"
    Case 3: d$="Wednesday"
    Case 4: d$="Thursday"
    Case 5: d$="Friday"
    Case 6: d$="Saturday"
    Default: d$="Sunday"
  EndSelect
  ProcedureReturn d$
EndProcedure

Procedure.s MonthInText()
  Protected  m$
  Select Month(Date())
    Case 1: m$="January"
    Case 2: m$="February"
    Case 3: m$="March"
    Case 4: m$="April"
    Case 5: m$="May"
    Case 6: m$="June"
    Case 7: m$="July"
    Case 8: m$="August"
    Case 9: m$="September"
    Case 10:m$="October"
    Case 11:m$="November"
    Default:m$="December"
  EndSelect
  ProcedureReturn m$
EndProcedure
