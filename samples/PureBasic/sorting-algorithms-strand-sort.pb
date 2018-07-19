Procedure strandSort(List a())
  Protected NewList subList()
  Protected NewList results()

  While ListSize(a()) > 0
    ClearList(subList())
    AddElement(subList())
    FirstElement(a())
    subList() = a()
    DeleteElement(a())
    ForEach a()
      If a() >= subList()
        AddElement(subList())
        subList() = a()
        DeleteElement(a())
      EndIf
    Next

    ;merge lists
    FirstElement(subList())
    If Not FirstElement(results())
      ;copy all of sublist() to results()
      MergeLists(subList(), results(), #PB_List_Last)
    Else
      Repeat
        If subList() < results()
          InsertElement(results())
          results() = subList()
          DeleteElement(subList())
          If Not NextElement(subList())
            Break
          EndIf
        ElseIf Not NextElement(results())
          ;add remainder of sublist() to end of results()
          MergeLists(subList(), results(), #PB_List_Last)
          Break
        EndIf
      ForEver
    EndIf

  Wend
  CopyList(results(), a())
EndProcedure

Procedure.s listContents(List a())
  Protected output.s
  PushListPosition(a())
  ForEach a()
    output + Str(a()) + ","
  Next
  PopListPosition(a())
  ProcedureReturn Left(output, Len(output) - 1)
EndProcedure

Procedure setupList(List a())
  ClearList(a())
  Protected elementCount, i

  elementCount = Random(5) + 10
  For i = 1 To elementCount
    AddElement(a())
    a() = Random(10) - 5
  Next
EndProcedure


If OpenConsole()
  NewList sample()
  Define i

  For i = 1 To 3
    setupList(sample())
    PrintN("List " + Str(i) + ":")
    PrintN("  Before:  " + listContents(sample()))
    strandSort(sample())
    PrintN("  After :  " + listContents(sample()))
    PrintN("")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
