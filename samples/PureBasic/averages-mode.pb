Procedure mean(Array InArray(1))

  Structure MyMean
    Value.i
    Cnt.i
  EndStructure

  Protected i, max, found
  Protected NewList MyDatas.MyMean()

  Repeat
    found=#False
    ForEach MyDatas()
      If InArray(i)=MyDatas()\Value
        MyDatas()\Cnt+1
        found=#True
        Break
      EndIf
    Next
    If Not found
      AddElement(MyDatas())
      MyDatas()\Value=InArray(i)
      MyDatas()\cnt+1
    EndIf
    If MyDatas()\Cnt>max
      max=MyDatas()\Cnt
    EndIf
    i+1
  Until i>ArraySize(InArray())

  ForEach MyDatas()
    If MyDatas()\Cnt=max
      For i=1 To max
        Print(Str(MyDatas()\Value)+" ")
      Next
    EndIf
  Next
EndProcedure
