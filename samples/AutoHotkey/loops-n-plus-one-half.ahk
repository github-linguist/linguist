Loop, 9                ; loop 9 times
{
  output .= A_Index    ; append the index of the current loop to the output var
  If (A_Index <> 9)    ; if it isn't the 9th iteration of the loop
    output .= ", "     ; append ", " to the output var
}
MsgBox, %output%
