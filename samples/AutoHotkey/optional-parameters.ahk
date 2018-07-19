Gosub start ; create and show the gui
sort_table("Text", column := 2, reverse := 1)  ;  lexicographic sort
Sleep, 2000
sort_table("Integer", column := 2, reverse := 1)  ;  numerical sort
Return

start:
  Gui, Add, ListView, r20 w200, 1|2|3
  data =
  (
  1,2,3
  b,q,z
  c,z,z
  )
  Loop, Parse, data, `n
  {
    StringSplit, row, A_LoopField, `,
    LV_Add(row, row1, row2, row3)
  }
  LV_ModifyCol(50)  ; Auto-size columns
  Gui, Show
Return

; The function supporting named, defaulted arguments
sort_table(ordering = "Text", column = 0, reverse = 0)
{
  If reverse
    desc = desc
  LV_ModifyCol(column, "sort" . desc . " " . ordering)
}

GuiClose:
  ExitApp
