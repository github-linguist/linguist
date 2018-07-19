MsgBox % stor(444)

stor(value)
{
  romans = M,CM,D,CD,C,XC,L,XL,X,IX,V,IV,I
  M := 1000
  CM := 900
  D := 500
  CD := 400
  C := 100
  XC := 90
  L := 50
  XL := 40
  X := 10
  IX := 9
  V := 5
  IV := 4
  I := 1
  Loop, Parse, romans, `,
  {
    While, value >= %A_LoopField%
    {
      result .= A_LoopField
      value := value - (%A_LoopField%)
    }
  }
  Return result . "O"
}
