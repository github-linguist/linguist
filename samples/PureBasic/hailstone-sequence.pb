NewList Hailstones.i() ; Make a linked list to use as we do not know the numbers of elements needed for an Array

Procedure.i FillHailstones(n) ; Fills the list & returns the amount of elements in the list
  Shared Hailstones()         ; Get access to the Hailstones-List
  ClearList(Hailstones())     ; Remove old data
  Repeat
    AddElement(Hailstones())  ; Add an element to the list
    Hailstones()=n            ; Fill current value in the new list element
    If n=1
      ProcedureReturn ListSize(Hailstones())
    ElseIf n%2=0
      n/2
    Else
      n=(3*n)+1
    EndIf
  ForEver
EndProcedure

If OpenConsole()
  Define i, l, maxl, maxi
  l=FillHailstones(27)
  Print("#27 has "+Str(l)+" elements and the sequence is: "+#CRLF$)
  ForEach Hailstones()
    If i=6
      Print(#CRLF$)
      i=0
    EndIf
    i+1
    Print(RSet(Str(Hailstones()),5))
    If Hailstones()<>1
      Print(", ")
    EndIf
  Next

  i=1
  Repeat
    l=FillHailstones(i)
    If l>maxl
      maxl=l
      maxi=i
    EndIf
    i+1
  Until i>=100000
  Print(#CRLF$+#CRLF$+"The longest sequence below 100000 is #"+Str(maxi)+", and it has "+Str(maxl)+" elements.")

  Print(#CRLF$+#CRLF$+"Press ENTER to exit."): Input()
  CloseConsole()
EndIf
