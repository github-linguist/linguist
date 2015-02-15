#STEP=2.2

Procedure Shell_sort(Array A(1))
  Protected l=ArraySize(A()), increment=Int(l/#STEP)
  Protected i, j, temp
  While increment
    For i= increment To l
      j=i
      temp=A(i)
      While j>=increment And A(j-increment)>temp
        A(j)=A(j-increment)
        j-increment
      Wend
      A(j)=temp
    Next i
    If increment=2
      increment=1
    Else
      increment*(5.0/11)
    EndIf
  Wend
EndProcedure
