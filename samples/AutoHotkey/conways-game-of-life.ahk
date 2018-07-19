rows := cols := 10                               ; set grid dimensions
i = -1,0,1, -1,1, -1,0,1                         ; neighbors' x-offsets
j = -1,-1,-1, 0,0, 1,1,1                         ; neighbors' y-offsets
StringSplit i, i, `,                             ; make arrays
StringSplit j, j, `,

Loop % rows {                                    ; setup grid of checkboxes
   r := A_Index, y := r*17-8                     ; looks good in VISTA
   Loop % cols {
      c := A_Index, x := c*17-5
      Gui Add, CheckBox, x%x% y%y% w17 h17 vv%c%_%r% gCheck
   }
}
Gui Add, Button, % "x12 w" x+2, step             ; button to step to next generation
Gui Show
Return

Check:
   GuiControlGet %A_GuiControl%                  ; manual set of cells
Return

ButtonStep:                                      ; move to next generation
   Loop % rows {
      r := A_Index
      Loop % cols {
         c := A_Index, n := 0
         Loop 8                                  ; w[x,y] <- new states
            x := c+i%A_Index%, y := r+j%A_Index%, n += 1=v%x%_%y%
         GuiControl,,v%c%_%r%,% w%c%_%r% := v%c%_%r% ? n=2 || n=3 : n=3
      }
   }
   Loop % rows {                                 ; update v[x,y] = states
      r := A_Index
      Loop % cols
         v%A_Index%_%r% := w%A_Index%_%r%
   }
Return

GuiClose:                                        ; exit when GUI is closed
ExitApp
