#SCALE = 10000
#ARRINT=  2000

Procedure Pi(Digits)
  Protected First=#True, Text$
  Protected Carry, i, j, sum
  Dim Arr(Digits)
  For i=0 To Digits
    Arr(i)=#ARRINT
  Next
  i=Digits
  While i>0
    sum=0
    j=i
    While j>0
      sum*j+#SCALE*arr(j)
      Arr(j)=sum%(j*2-1)
      sum/(j*2-1)
      j-1
    Wend
    Text$ = RSet(Str(Carry+sum/#SCALE),4,"0")
    If First
      Text$ = ReplaceString(Text$,"3","3.")
      First = #False
    EndIf
    Print(Text$)
    Carry=sum%#SCALE
    i-14
  Wend
EndProcedure

If OpenConsole()
  SetConsoleCtrlHandler_(?Ctrl,#True)
  Pi(24*1024*1024)
EndIf
End

Ctrl:
PrintN(#CRLF$+"Ctrl-C was pressed")
End
