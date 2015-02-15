Structure Employees
  Name$
  ID$
  Salary.i
  Department$
EndStructure

Procedure displayTopEarners(List MyEmployees.Employees(), n)
  Protected filename$ = OpenFileRequester("Top rank per group", "DataFile.txt", "", 0)
  If ReadFile(0, filename$)
    Protected InData.Employees, txt.s, MaxNameLength

    While Eof(0) = 0
      AddElement(MyEmployees())
      txt = ReadString(0)
      With MyEmployees()
        \Name$ = StringField(txt, 1, ",")
        \ID$ = StringField(txt, 2, ",")
        \Salary = Val(StringField(txt, 3, ","))
        \Department$ = StringField(txt, 4, ",")
        If Len(\Name$) > MaxNameLength: MaxNameLength = Len(\Name$): EndIf
      EndWith
    Wend
    CloseFile(0)
  Else
    MessageRequester("Information", "Couldn't open the file!")
    End
  EndIf

  If OpenConsole()
    Protected OldDepartment$, count

    SortStructuredList(MyEmployees(), #PB_Sort_Descending, OffsetOf(Employees\Salary), #PB_Sort_integer)
    SortStructuredList(MyEmployees(), #PB_Sort_Ascending, OffsetOf(Employees\Department$), #PB_Sort_String)
    ForEach MyEmployees()
      With MyEmployees()
        If \Department$ <> OldDepartment$
          If OldDepartment$ <> ""
            PrintN(#CRLF$)
          EndIf
          OldDepartment$ = \Department$
          PrintN("Department " + \Department$ + #CRLF$ + "---------------")
          PrintN(LSet("Name", MaxNameLength + 3) + LSet("ID", 7) + LSet("Salary", 7))
          count = 0
        EndIf
        count + 1
        If count <= n
          PrintN(LSet(\Name$, MaxNameLength + 1) + " " + RSet(\ID$, 7) + " $" + Str(\Salary))
        EndIf
      EndWith
    Next
    PrintN(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  EndIf
EndProcedure

NewList MyEmployees.Employees()

displayTopEarners(MyEmployees(), 3)
