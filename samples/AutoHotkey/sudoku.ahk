#SingleInstance, Force
SetBatchLines, -1
SetTitleMatchMode, 3

    Loop 9 {
       r := A_Index, y := r*17-8 + (A_Index >= 7 ? 4 : A_Index >= 4 ? 2 : 0)
       Loop 9 {
          c := A_Index, x := c*17+5 + (A_Index >= 7 ? 4 : A_Index >= 4 ? 2 : 0)
          Gui, Add, Edit, x%x% y%y% w17 h17 v%r%_%c% Center Number Limit1 gNext
       }
    }
    Gui, Add, Button, vButton gSolve w175 x10 Center, Solve
    Gui, Add, Text, vMsg r3, Enter Sudoku puzzle and click Solve
    Gui, Show,, Sudoku Solver
Return

Solve:
    Gui, Submit, NoHide
    Loop 9
    {
       r := A_Index
       Loop 9
          If (%r%_%A_Index% = "")
             puzzle .= "@"
          Else
             puzzle .= %r%_%A_Index%
    }
    s := A_TickCount
    answer := Sudoku(puzzle)
    iterations := ErrorLevel
    e := A_TickCount
    seconds := (e-s)/1000
    StringSplit, a, answer, |
    Loop 9
    {
       r := A_Index
       Loop 9
       {
          b := (r*9)+A_Index-9
          GuiControl,, %r%_%A_Index%, % a%b%
          GuiControl, +ReadOnly, %r%_%A_Index%
        }
    }
    if answer
        GuiControl,, Msg, Solved!`nTime: %seconds%s`nIterations: %iterations%
    else
        GuiControl,, Msg, Failed! :(`nTime: %seconds%s`nIterations: %iterations%
    GuiControl,, Button, Again!
    GuiControl, +gAgain, Button
return

GuiClose:
    ExitApp

Again:
    Reload

#IfWinActive, Sudoku Solver
~*Enter::GoSub % GetKeyState( "Shift", "P" ) ? "~Up" : "~Down"
~Up::
    GuiControlGet, f, focus
    StringTrimLeft, f, f, 4
    f := ((f >= 1 && f <= 9) ? f+72 : f-9)
    GuiControl, Focus, Edit%f%
return
~Down::
    GuiControlGet, f, focus
    StringTrimLeft, f, f, 4
    f := ((f >= 73 && f <= 81) ? f-72 : f + 9)
    GuiControl, Focus, Edit%f%
return
~Left::
    GuiControlGet, f, focus
    StringTrimLeft, f, f, 4
    f := Mod(f + 79, 81) + 1
    GuiControl, Focus, Edit%f%
return
Next:
~Right::
    GuiControlGet, f, focus
    StringTrimLeft, f, f, 4
    f := Mod(f, 81) + 1
    GuiControl, Focus, Edit%f%
return
#IfWinActive

; Functions Start here

Sudoku( p ) { ;ErrorLevel contains the number of iterations
   p := RegExReplace(p, "[^1-9@]"), ErrorLevel := 0 ;format puzzle as single line string
   return Sudoku_Display(Sudoku_Solve(p))
}

Sudoku_Solve( p, d = 0 ) { ;d is 0-based
;   http://www.autohotkey.com/forum/topic46679.html
;   p: 81 character puzzle string
;      (concat all 9 rows of 9 chars each)
;      givens represented as chars 1-9
;      fill-ins as any non-null, non 1-9 char
;   d: used internally. omit on initial call
;
;   returns: 81 char string with non-givens replaced with valid solution
;
   If (d >= 81), ErrorLevel++
      return p  ;this is 82nd iteration, so it has successfully finished iteration 81
   If InStr( "123456789", SubStr(p, d+1, 1) ) ;this depth is a given, skip through
      return Sudoku_Solve(p, d+1)
   m := Sudoku_Constraints(p,d) ;a string of this level's constraints.
   ; (these will not change for all 9 loops)
   Loop 9
   {
      If InStr(m, A_Index)
         Continue
      NumPut(Asc(A_Index), p, d, "Char")
      If r := Sudoku_Solve(p, d+1)
         return r
   }
   return 0
}

Sudoku_Constraints( ByRef p, d ) {
; returns a string of the constraints for a particular position
     c := Mod(d,9)
   , r := (d - c) // 9
   , b := r//3*27 + c//3*3 + 1
   ;convert to 1-based
   , c++
   return ""
   ; row:
      . SubStr(p, r * 9 + 1, 9)
   ; column:
      . SubStr(p,c   ,1) SubStr(p,c+9 ,1) SubStr(p,c+18,1)
      . SubStr(p,c+27,1) SubStr(p,c+36,1) SubStr(p,c+45,1)
      . SubStr(p,c+54,1) SubStr(p,c+63,1) SubStr(p,c+72,1)
   ;box
      . SubStr(p, b, 3) SubStr(p, b+9, 3) SubStr(p, b+18, 3)
}

Sudoku_Display( p ) {
   If StrLen(p) = 81
      loop 81
         r .= SubStr(p, A_Index, 1) . "|"
   return r
}
