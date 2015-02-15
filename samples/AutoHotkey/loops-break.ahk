Loop
{
  Random, var, 0, 19
  output = %output%`n%var%
  If (var = 10)
    Break
  Random, var, 0, 19
  output = %output%`n%var%
}
MsgBox % output
