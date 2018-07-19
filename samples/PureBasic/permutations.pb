Macro reverse(firstIndex, lastIndex)
  first = firstIndex
  last = lastIndex
  While first < last
    Swap cur(first), cur(last)
    first + 1
    last - 1
  Wend
EndMacro

Procedure nextPermutation(Array cur(1))
  Protected first, last, elementCount = ArraySize(cur())
  If elementCount < 1
    ProcedureReturn #False ;nothing to permute
  EndIf

  ;Find the lowest position pos such that [pos] < [pos+1]
  Protected pos = elementCount - 1
  While cur(pos) >= cur(pos + 1)
    pos - 1
    If pos < 0
      reverse(0, elementCount)
      ProcedureReturn #False ;no higher lexicographic permutations left, return lowest one instead
    EndIf
  Wend

  ;Swap [pos] with the highest positional value that is larger than [pos]
  last = elementCount
  While cur(last) <= cur(pos)
    last - 1
  Wend
  Swap cur(pos), cur(last)

  ;Reverse the order of the elements in the higher positions
  reverse(pos + 1, elementCount)
  ProcedureReturn #True ;next lexicographic permutation found
EndProcedure

Procedure display(Array a(1))
  Protected i, fin = ArraySize(a())
  For i = 0 To fin
    Print(Str(a(i)))
    If i = fin: Continue: EndIf
    Print(", ")
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  Dim a(2)
  a(0) = 1: a(1) = 2: a(2) =  3
  display(a())
  While nextPermutation(a()): display(a()): Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
