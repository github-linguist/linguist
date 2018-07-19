Procedure DateG(year.w, month.b, day)
  ;Returns the number of days before or after the earliest reference date
  ;in PureBasic's Date Library (1 Jan 1970) based on an assumed Gregorian calendar calculation
  Protected days
  days = (year) * 365 + (month - 1) * 31 + day - 1 - 719527 ;DAYS_UNTIL_1970_01_01 = 719527
  If month >= 3
    days - Int(0.4 * month + 2.3)
  Else
    year - 1
  EndIf
  days + Int(year/4) - Int(year/100) + Int(year/400)

  ProcedureReturn days
EndProcedure

Procedure startsOnFriday(year, month)
  ;0 is Sunday, 1 is Monday, ... 5 is Friday, 6 is Saturday
  Protected referenceDay = DayOfWeek(Date(1970, 1, 1, 0, 0, 0)) ;link to the first day in the PureBasic's date library
  Protected resultDay = (((DateG(year, month, 1) + referenceDay) % 7) + 7) % 7
   If resultDay = 5
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure has31Days(month)
  Select month
    Case 1, 3, 5, 7 To 8, 10, 12
      ProcedureReturn #True
  EndSelect
EndProcedure

Procedure checkMonths(year)
  Protected month, count
  For month = 1 To 12
    If startsOnFriday(year, month) And has31Days(month)
      count + 1
      PrintN(Str(year) + " " + Str(month))
    EndIf
  Next
  ProcedureReturn count
EndProcedure

Procedure fiveWeekends()
  Protected startYear = 1900, endYear = 2100, year, monthTotal, total
  NewList yearsWithoutFiveWeekends()

  For year = startYear To endYear
    monthTotal = checkMonths(year)
    total + monthTotal
    ;extra credit
    If monthTotal = 0
      AddElement(yearsWithoutFiveWeekends())
      yearsWithoutFiveWeekends() = year
    EndIf
  Next

   PrintN("Total number of months: " + Str(total) + #CRLF$)
   PrintN("Years with no five-weekend months: " + Str(ListSize(yearsWithoutFiveWeekends())) )
EndProcedure

If OpenConsole()
  fiveWeekends()

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
