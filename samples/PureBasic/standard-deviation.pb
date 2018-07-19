;Define our Standard deviation function
Declare.d Standard_deviation(x)

; Main program
If OpenConsole()
  Define i, x
  Restore MyList
  For i=1 To 8
    Read.i x
    PrintN(StrD(Standard_deviation(x)))
  Next i
  Print(#CRLF$+"Press ENTER to exit"): Input()
EndIf

;Calculation procedure, with memory
Procedure.d Standard_deviation(In)
  Static in_summa, antal
  Static in_kvadrater.q
  in_summa+in
  in_kvadrater+in*in
  antal+1
  ProcedureReturn Pow((in_kvadrater/antal)-Pow(in_summa/antal,2),0.50)
EndProcedure

;data section
DataSection
MyList:
  Data.i  2,4,4,4,5,5,7,9
EndDataSection
