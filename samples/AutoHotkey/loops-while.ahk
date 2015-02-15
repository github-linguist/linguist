i = 1024
While (i > 0)
{
  output = %output%`n%i%
  i := Floor(i / 2)
}
MsgBox % output
