#MAXNUM=100

Dim MyData(Random(15)+5)
Global Dim Abacus(0,0)

Declare BeadSort(Array InData(1))
Declare PresentData(Array InData(1))

If OpenConsole()
  Define i
  ;- Generate a random array
  For i=0 To ArraySize(MyData())
    MyData(i)=Random(#MAXNUM)
  Next i
  PresentData(MyData())
  ;
  ;- Sort the array
  BeadSort(MyData())
  PresentData(MyData())
  ;
  Print("Press ENTER to exit"): Input()
EndIf

Procedure LetFallDown(x)
  Protected y=ArraySize(Abacus(),2)-1
  Protected ylim=y
  While y>=0
    If Abacus(x,y) And Not Abacus(x,y+1)
      Swap Abacus(x,y), Abacus(x,y+1)
      If y<ylim: y+1: Continue: EndIf
    Else
      y-1
    EndIf
  Wend
EndProcedure

Procedure BeadSort(Array n(1))
  Protected i, j, k
  NewList T()
  Dim Abacus(#MAXNUM,ArraySize(N()))
  ;- Set up the abacus
  For i=0 To ArraySize(Abacus(),2)
    For j=1 To N(i)
      Abacus(j,i)=#True
    Next
  Next
  ;- sort it in threads to simulate free beads falling down
  For i=0 To #MAXNUM
    AddElement(T()): T()=CreateThread(@LetFallDown(),i)
  Next
  ForEach T()
    WaitThread(T())
  Next
  ;- send it back to a normal array
  For j=0 To ArraySize(Abacus(),2)
    k=0
    For i=0 To ArraySize(Abacus())
      k+Abacus(i,j)
    Next
    N(j)=k
  Next
EndProcedure

Procedure PresentData(Array InData(1))
  Protected n, m, sum
  PrintN(#CRLF$+"The array is;")
  For n=0 To ArraySize(InData())
    m=InData(n): sum+m
    Print(Str(m)+" ")
  Next
  PrintN(#CRLF$+"And its sum= "+Str(sum))
EndProcedure
