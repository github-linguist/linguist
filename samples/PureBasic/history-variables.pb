; integer history variable

Structure historyint
    List value.i()
EndStructure

Procedure SetInt (*var.historyint, val.i)
    AddElement(*var\value())

    If (ListSize(*var\value()) = 1)
        *var\value() = 0
        AddElement(*var\value())

    EndIf

    *var\value() = val
EndProcedure

Procedure ShowHistory (*var.historyint)
    ForEach *var\value()
        Debug *var\value()
    Next
EndProcedure

Procedure UndoInt (*var.historyint)
    If (ListSize(*var\value()) = 1)
        ProcedureReturn
    EndIf

    DeleteElement(*var\value())
EndProcedure

;----------------------------------------------

Define x.historyint

For i = 0 To 3
    setint(x, Random(50)+100 )
Next

Debug "x history:"
ShowHistory(x)

Debug ""

For i = 0 To 3
    UndoInt(x)
    Debug "undo, x = "+Str(x\value())
Next
