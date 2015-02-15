MsgBox % key := rle_encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
MsgBox % rle_decode(key)

rle_encode(message)
{
  StringLeft, previous, message, 1
  StringRight, last, message, 1
  message .= Asc(Chr(last)+1)
  count = 0
  Loop, Parse, message
  {
    If (previous == A_LoopField)
      count +=1
    Else
    {
      output .= previous . count
      previous := A_LoopField
      count = 1
    }
  }
  Return output
}

rle_decode(message)
{
  pos = 1
  While, item := RegExMatch(message, "\D", char, pos)
  {
    digpos := RegExMatch(message, "\d+", dig, item)
    Loop, % dig
      output .= char
    pos := digpos
  }
  Return output
}
