lines =
(
|$|$|$|$|$|$|$|$|$|$|$|
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
)

Clipboard := ColumnJustify(lines, "l")

MsgBox, , Column Justify, The clipboard now contains the justified text. Paste it into a text editor to see it.

ColumnJustify(lines, lcr = "l", del="$")
{
  Loop, Parse, lines, `n, `r
    Loop, Parse, A_LoopField, %del%
    {
      If ((t := StrLen(A_LoopField)) > c%A_Index% )
        c%A_Index% :=  t
      If (t > max)
        max := t
    }
  blank := Fill( " ", max )
  If (lcr = "l") ;left-justify
    Loop, Parse, lines, `n, `r
      Loop, Parse, A_LoopField, %del%
        out .= (A_Index = 1 ? "`n" : " ") SubStr(A_LoopField blank, 1, c%A_Index%)
  Else If (lcr = "r") ;right-justify
    Loop, Parse, lines, `n, `r
      Loop, Parse, A_LoopField, %del%
        out .= (A_Index = 1 ? "`n" : " ") SubStr(blank A_LoopField, -c%A_Index%+1)
  Else If (lcr = "c") ;center-justify
    Loop, Parse, lines, `n, `r
      Loop, Parse, A_LoopField, %del%
        out .= (A_Index = 1 ? "`n" : " ") SubStr(blank A_LoopField blank
          , (Ceil((max * 2 + StrLen(A_LoopField))/2) - Ceil(c%A_Index%/2) + 1)
          , c%A_Index%)
  return SubStr(out, 2)
}

Fill(chr, len)
{
  static y
  if !y
    VarSetCapacity(x, 64), VarSetCapacity(x, 0), y := True
  return x, VarSetCapacity(x, len, Asc(chr))
}
