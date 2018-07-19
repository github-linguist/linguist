Procedure.s getCode(c.s)
    Protected  getCode.s = ""

    If FindString("BFPV", c ,1)     : getCode = "1" : EndIf
    If FindString("CGJKQSXZ", c ,1) : getCode = "2" : EndIf
    If FindString("DT", c ,1)       : getCode = "3" : EndIf
    If "L" = c                      : getCode = "4" : EndIf
    If FindString("MN", c ,1)       : getCode = "5" : EndIf
    If "R" = c                      : getCode = "6" : EndIf
    If FindString("HW", c ,1)       : getCode = "." : EndIf
    ProcedureReturn getCode
EndProcedure

Procedure.s soundex(word.s)
    Protected.s previous.s = "" , code.s , current , soundex
    Protected.i i

    word = UCase(word)
    code = Mid(word,1,1)
    previous = ""
    For i = 2 To (Len(word) + 1)
        current = getCode(Mid(word, i, 1))
        If current = "." : Continue : EndIf
        If Len(current) > 0 And current <> previous
            code + current
        EndIf
        previous = current
        If Len(code) = 4
          Break
        EndIf
    Next
    If Len(code) < 4
        code = LSet(code, 4,"0")
    EndIf
    ProcedureReturn code
EndProcedure

OpenConsole()

PrintN (soundex("Lukasiewicz"))
PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
