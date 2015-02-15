If OpenConsole()
  Define.i upperlim, i=1, k1=2, n=3, v=1
  Define.d Maximum
  Print("Enter limit (ENTER gives 2^20=1048576): "): upperlim=Val(Input())
  If upperlim<=0: upperlim=1048576: EndIf
  Dim tal(upperlim)
  If ArraySize(tal())=-1
    PrintN("Could not allocate needed memory!"): Input(): End
  EndIf
  tal(1)=1: tal(2)=1
  While n<=upperlim
    v=tal(v)+tal(n-v)
    tal(n)=v
    If Maximum<(v/n): Maximum=v/n: EndIf
    If Not n&k1
      PrintN("Maximum between 2^"+Str(i)+" and 2^"+Str(i+1)+" was "+StrD(Maximum,6))
      Maximum=0.0
      i+1
    EndIf
    k1=n
    n+1
  Wend

  Print(#CRLF$+"Press ENTER to exit."): Input()
  CloseConsole()
EndIf
