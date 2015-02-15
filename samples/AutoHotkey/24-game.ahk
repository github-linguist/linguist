AutoExecute:
    Title := "24 Game"
    Gui, -MinimizeBox
    Gui, Add, Text, w230 vPuzzle
    Gui, Add, Edit, wp vAnswer
    Gui, Add, Button, w70, &Generate
    Gui, Add, Button, x+10 wp Default, &Submit
    Gui, Add, Button, x+10 wp, E&xit


ButtonGenerate: ; new set of numbers
    Loop, 4
        Random, r%A_Index%, 1, 9
    Puzzle = %r1%, %r2%, %r3%, and %r4%
    GuiControl,, Puzzle, The numbers are:  %Puzzle%  - Good luck!
    GuiControl,, Answer ; empty the edit box
    ControlFocus, Edit1
    Gui, -Disabled
    Gui, Show,, %Title%
Return ; end of auto execute section


ButtonSubmit: ; check solution
    Gui, Submit, NoHide
    Gui, +Disabled

    ; check numbers used
    RegExMatch(Answer, "(\d)\D+(\d)\D+(\d)\D+(\d)", $)
    ListPuzzle := r1 "," r2 "," r3 "," r4
    ListAnswer := $1 "," $2 "," $3 "," $4
    Sort, ListPuzzle, D,
    Sort, ListAnswer, D,
    If Not ListPuzzle = ListAnswer {
        MsgBox, 48, Error - %Title%, Numbers used!`n%Answer%
        Goto, TryAgain
    }

    ; check operators used
    StringReplace, $, $, +,, All
    StringReplace, $, $, -,, All
    StringReplace, $, $, *,, All
    StringReplace, $, $, /,, All
    StringReplace, $, $, (,, All
    StringReplace, $, $, ),, All
    Loop, 9
        StringReplace, $, $, %A_Index%,, All
    If StrLen($) > 0
    Or InStr(Answer, "**")
    Or InStr(Answer, "//")
    Or InStr(Answer, "++")
    Or InStr(Answer, "--") {
        MsgBox, 48, Error - %Title%, Operators used!`n%Answer%
        Goto, TryAgain
    }

    ; check result
    Result := Eval(Answer)
    If Not Result = 24 {
        MsgBox, 48, Error - %Title%, Result incorrect!`n%Result%
        Goto, TryAgain
    }

    ; if we are sill here
    MsgBox, 4, %Title%, Correct solution! Play again?
    IfMsgBox, Yes
        Gosub, ButtonGenerate
    Else
        ExitApp
Return


TryAgain: ; alternative ending of routine ButtonSubmit
    ControlFocus, Edit1
    Gui, -Disabled
    Gui, Show
Return


GuiClose:
GuiEscape:
ButtonExit:
    ExitApp
Return


;---------------------------------------------------------------------------
Eval(Expr) { ; evaluate expression using separate AHK process
;---------------------------------------------------------------------------
    ; credit for this function goes to AutoHotkey forum member Laszlo
    ; http://www.autohotkey.com/forum/topic9578.html
    ;-----------------------------------------------------------------------
    static File := "24$Temp.ahk"

    ; delete old temporary file, and write new
    FileDelete, %File%
    FileContent := "#NoTrayIcon`r`n"
                .  "FileDelete, " File "`r`n"
                .  "FileAppend, `% " Expr ", " File "`r`n"
    FileAppend, %FileContent%, %File%

    ; run AHK to execute temp script, evaluate expression
    RunWait, %A_AhkPath% %File%

    ; get result
    FileRead, Result, %File%
    FileDelete, %File%
    Return, Result
}
