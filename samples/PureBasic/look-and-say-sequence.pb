If OpenConsole()
  Define i, j, cnt, txt$, curr$, result$
  Print("Enter start sequence: "): txt$=Input()
  Print("How many repetitions: "): i=Val(Input())
  ;
  PrintN(#CRLF$+"Sequence:"+#CRLF$+txt$)
  Repeat
    j=1
    result$=""
    Repeat
      curr$=Mid(txt$,j,1)
      cnt=0
      Repeat
        cnt+1
        j+1
      Until Mid(txt$,j,1)<>curr$
      result$+Str(cnt)+curr$
    Until j>Len(txt$)
    PrintN(result$)
    txt$=result$
    i-1
  Until i<=0
  ;
  PrintN(#CRLF$+"Press ENTER to exit."): Input()
  CloseConsole()
EndIf
