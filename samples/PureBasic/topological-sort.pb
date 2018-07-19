#EndOfDataMarker$ = "::EndOfData::"
DataSection
  ;"LIBRARY: [LIBRARY_DEPENDENCY_1 LIBRARY_DEPENDENCY_2 ... LIBRARY_DEPENDENCY_N]
  Data.s "des_system_lib: [std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee]"
  Data.s "dw01: [ieee dw01 dware gtech]"
  ;Data.s "dw01: [ieee dw01 dware gtech dw04]" ;comment the previous line and uncomment this one for cyclic dependency
  Data.s "dw02: [ieee dw02 dware]"
  Data.s "dw03: [std synopsys dware dw03 dw02 dw01 ieee gtech]"
  Data.s "dw04: [dw04 ieee dw01 dware gtech]"
  Data.s "dw05: [dw05 ieee dware]"
  Data.s "dw06: [dw06 ieee dware]"
  Data.s "dw07: [ieee dware]"
  Data.s "dware: [ieee dware]"
  Data.s "gtech: [ieee gtech]"
  Data.s "ramlib: [std ieee]"
  Data.s "std_cell_lib: [ieee std_cell_lib]"
  Data.s "synopsys: nil"
  Data.s #EndOfDataMarker$
EndDataSection

Structure DAG_node
  Value.s
  forRemoval.i ;flag marks elements that should be removed the next time they are accessed
  List dependencies.s()
EndStructure

If Not OpenConsole()
  MessageRequester("Error","Unable to open console")
  End
EndIf

;// initialize Directed Acyclic Graph //
Define i, itemData.s, firstBracketPos
NewList DAG.DAG_node()
Repeat
  Read.s itemData
  itemData = Trim(itemData)
  If itemData <> #EndOfDataMarker$
    AddElement(DAG())
    ;add library
    DAG()\Value = Trim(Left(itemData, FindString(itemData, ":", 1) - 1))
    ;parse library dependencies
    firstBracketPos = FindString(itemData, "[", 1)
    If firstBracketPos
      itemData = Trim(Mid(itemData, firstBracketPos + 1, FindString(itemData, "]", 1) - firstBracketPos - 1))
      For i = (CountString(itemData, " ") + 1) To 1 Step -1
        AddElement(DAG()\dependencies())
        DAG()\dependencies() = StringField(itemData, i, " ")
      Next
    EndIf
  EndIf
Until itemData = #EndOfDataMarker$

;// process DAG //
;create DAG entry for nodes listed in dependencies but without their own entry
NewMap libraries()
ForEach DAG()
  ForEach DAG()\dependencies()
    libraries(DAG()\dependencies()) = #True
    If DAG()\dependencies() = DAG()\Value
      DeleteElement(DAG()\dependencies()) ;remove self-dependencies
    EndIf
  Next
Next

ForEach DAG()
  If FindMapElement(libraries(),DAG()\Value)
    DeleteMapElement(libraries(),DAG()\Value)
  EndIf
Next

ResetList(DAG())
ForEach libraries()
  AddElement(DAG())
  DAG()\Value = MapKey(libraries())
Next
ClearMap(libraries())

;process DAG() repeatedly until no changes occur
NewList compileOrder.s()
Repeat
  noChangesMade = #True
  ForEach DAG()
    If DAG()\forRemoval
      DeleteElement(DAG())
    Else
      ;remove dependencies that have been placed in the compileOrder
      ForEach DAG()\dependencies()
        If FindMapElement(libraries(),DAG()\dependencies())
          DeleteElement(DAG()\dependencies())
        EndIf
      Next
      ;add DAG() entry to compileOrder if it has no more dependencies
      If ListSize(DAG()\dependencies()) = 0
        AddElement(compileOrder())
        compileOrder() = DAG()\Value
        libraries(DAG()\Value) = #True ;mark the library for removal as a dependency
        DAG()\forRemoval = #True
        noChangesMade = #False
      EndIf
    EndIf
  Next
Until noChangesMade

If ListSize(DAG())
  PrintN("Cyclic dependencies detected in:" + #CRLF$)
  ForEach DAG()
    PrintN(" " + DAG()\Value)
  Next
Else
  PrintN("Compile order:" + #CRLF$)
  ForEach compileOrder()
    PrintN(" " + compileOrder())
  Next
EndIf

Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
Input()
CloseConsole()
