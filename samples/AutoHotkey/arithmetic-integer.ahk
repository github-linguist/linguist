Gui, Add, Edit, va, 5
Gui, Add, Edit, vb, -3
Gui, Add, Button, Default, Compute
Gui, Show
Return

ButtonCompute:
  Gui, Submit
  MsgBox,%
  (Join`s"`n"
   a "+" b " = " a+b
   a "-" b " = " a-b
   a "*" b " = " a*b
   a "//" b " = " a//b " remainder " Mod(a,b)
   a "**" b " = " a**b
  )
; fallthrough
GuiClose:
  ExitApp
