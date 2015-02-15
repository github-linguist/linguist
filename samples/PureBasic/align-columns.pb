Declare max(a,b)

If OpenConsole()
  Define a, i, x, y, maxlen
  Dim txt.s(0)
  Restore lines             ; Get address of the first data block
  Read.i  a
  ReDim txt(a)
  For i=0 To a              ; Read the raw data lines
    Read.s txt(i)
    txt(i)=Trim(txt(i),"$") ; Remove any bad '$' that may be useless in the end...
    x=max(CountString(txt(i),"$"),x)
  Next
  y=a
  Dim matrix.s(x,y)         ; Set up a nice matrix to work with, each word cleanly separated
  For x=0 To ArraySize(matrix(),1)
    For y=0 To ArraySize(matrix(),2)
      matrix(x,y)=StringField(txt(y),x+1,"$")
      maxlen=max(maxlen,Len(matrix(x,y)))
    Next
  Next
  If maxlen%2
    maxlen+1                ; Just to make sure that 'centered' output looks nice....
  EndIf

  PrintN(#CRLF$+"---- Right ----")
  For y=0 To ArraySize(matrix(),2)
    For x=0 To ArraySize(matrix(),1)
      Print(RSet(matrix(x,y),maxlen+1))
    Next
    PrintN("")
  Next

  PrintN(#CRLF$+"---- Left ----")
  For y=0 To ArraySize(matrix(),2)
    For x=0 To ArraySize(matrix(),1)
      Print(LSet(matrix(x,y),maxlen+1))
    Next
    PrintN("")
  Next

  PrintN(#CRLF$+"---- Center ----")
  For y=0 To ArraySize(matrix(),2)
    For x=0 To ArraySize(matrix(),1)
      a=maxlen-Len(matrix(x,y))
      Print(LSet(RSet(matrix(x,y),maxlen-a/2),maxlen))
    Next
    PrintN("")
  Next

  PrintN(#CRLF$+#CRLF$+"Press ENTER to quit."): Input()
  CloseConsole()
EndIf


Procedure max(x,y)
  If x>=y
    ProcedureReturn x
  Else
    ProcedureReturn y
  EndIf
EndProcedure


DataSection
lines:
  Data.i  5 ; e.g. 6-1 since first line is equal to 'zero'.
text:
  Data.s "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
  Data.s "are$delineated$by$a$single$'dollar'$character,$write$a$program"
  Data.s "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
  Data.s "column$are$separated$by$at$least$one$space."
  Data.s "Further,$allow$for$each$word$in$a$column$oo$be$either$left$"
  Data.s "justified,$right$justified,$or$center$justified$within$its$column."
EndDataSection
