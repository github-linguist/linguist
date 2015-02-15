Gui, Add, Edit, vOutput r5 w100 -VScroll ; Create an Edit-Control
Gui, Show ; Show the window
Loop, 5 ; loop 5 times
{
  Loop, %A_Index% ; A_Index contains the Index of the current loop
  {
    output .= "*" ; append an "*" to the output var
    GuiControl, , Output, %Output% ; update the Edit-Control with the new content
    Sleep, 500 ; wait some(500ms) time, [just to show off]
  }
  Output .= (A_Index = 5) ? "" : "`n" ; append a new line to the output if A_Index is not "5"
}
Return ; End of auto-execution section
