n := 22, n1 := n+1, v0 := v%n1% := 0        ; set grid dimensions, and fixed cells

Loop % n {                                  ; draw a line of checkboxes
   v%A_Index% := 0
   Gui Add, CheckBox, % "y10 w17 h17 gCheck x" A_Index*17-5 " vv" A_Index
}
Gui Add, Button, x+5 y6, step               ; button to step to next generation
Gui Show
Return

Check:
   GuiControlGet %A_GuiControl%             ; set cells by the mouse
Return

ButtonStep:                                 ; move to next generation
   Loop % n
      i := A_Index-1, j := i+2, w%A_Index% := v%i%+v%A_Index%+v%j% = 2
   Loop % n
      GuiControl,,v%A_Index%, % v%A_Index% := w%A_Index%
Return

GuiClose:                                   ; exit when GUI is closed
ExitApp
