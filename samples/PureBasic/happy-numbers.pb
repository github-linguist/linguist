#ToFind=8
#MaxTests=100
#True = 1: #False = 0
Declare is_happy(n)

If OpenConsole()
  Define i=1,Happy
  Repeat
    If is_happy(i)
      Happy+1
      PrintN("#"+Str(Happy)+RSet(Str(i),3))
    EndIf
    i+1
  Until Happy>=#ToFind
  ;
  Print(#CRLF$+#CRLF$+"Press ENTER to exit"): Input()
  CloseConsole()
EndIf

Procedure is_happy(n)
  Protected i,j=n,dig,sum
  Repeat
    sum=0
    While j
      dig=j%10
      j/10
      sum+dig*dig
    Wend
    If sum=1: ProcedureReturn #True: EndIf
    j=sum
    i+1
  Until i>#MaxTests
  ProcedureReturn #False
EndProcedure
