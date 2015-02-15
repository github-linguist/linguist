Procedure Cube(Array param.i(1))
    Protected n.i
    For n = 0 To ArraySize(param())
        Debug Str(param(n)) + "^3 = " + Str(param(n) * param(n) * param(n))
    Next
EndProcedure

Dim AnArray.i(4)

For n = 0 To ArraySize(AnArray())
    AnArray(n) = Random(99)
Next

Cube(AnArray())
