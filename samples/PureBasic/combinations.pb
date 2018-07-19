Procedure.s Combinations(amount, choose)
  NewList comb.s()
  ; all possible combinations with {amount} Bits
  For a = 0 To 1 << amount
    count = 0
    ; count set bits
    For x = 0 To amount
      If (1 << x)&a
        count + 1
      EndIf
    Next
    ; if set bits are equal to combination length
    ; we generate a String representing our combination and add it to list
    If count = choose
      string$ = ""
      For x = 0 To amount
        If (a >> x)&1
          ; replace x by x+1 to start counting with 1
          String$ + Str(x) + " "
        EndIf
      Next
      AddElement(comb())
      comb() = string$
    EndIf
  Next
  ; now we sort our list and format it for output as string
  SortList(comb(), #PB_Sort_Ascending)
  ForEach comb()
    out$ + ", [ " + comb() + "]"
  Next
  ProcedureReturn Mid(out$, 3)
EndProcedure

Debug Combinations(5, 3)
