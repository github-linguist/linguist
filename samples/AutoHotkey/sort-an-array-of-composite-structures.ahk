start:
Gui, Add, ListView, r20 w200, 1|2
data =
(
foo,53
joe,34
bar,23
)

Loop, parse, data, `n
{
  stringsplit, row, A_LoopField, `,
  LV_Add(row, row1, row2)
}
LV_ModifyCol()  ; Auto-size columns
Gui, Show
msgbox, sorting by column1
LV_ModifyCol(1, "sort") ; sort by first column
msgbox, sorting by column2
LV_ModifyCol(2, "sort Integer") ; sort by second column numerically
return

GuiClose:
ExitApp
