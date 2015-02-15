MsgBox % Repeat("ha",5)

Repeat(String,Times)
{
  Loop, %Times%
    Output .= String
  Return Output
}
