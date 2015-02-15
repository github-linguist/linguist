MsgBox, calling Label1
Gosub, Label1
MsgBox, Label1 subroutine finished
Goto Label2
MsgBox, calling Label2 ; this part is never reached
Return

Label1:
  MsgBox, Label1
Return

Label2:
  MsgBox, Label2 will not return to calling routine
Return
