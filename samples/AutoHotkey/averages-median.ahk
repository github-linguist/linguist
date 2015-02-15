seq = 4.1, 7.2, 1.7, 9.3, 4.4, 3.2, 5
MsgBox % median(seq, "`,")  ; 4.1

median(seq, delimiter)
{
  Sort, seq, ND%delimiter%
  StringSplit, seq, seq, % delimiter
  median := Floor(seq0 / 2)
  Return seq%median%
}
