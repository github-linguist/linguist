Loop, 10
{
  i := A_Index
  Loop, 10
  {
    j := A_Index
    Random, a%i%%j%, 1, 20
  }
}

Loop, 10
{
  i := A_Index
  Loop, 10
  {
    j := A_Index
    If (a%i%%j% == 20)
      Goto finish
  }
}

finish:
  MsgBox % "a[" . i . "][" . j . "]" is 20
Return
