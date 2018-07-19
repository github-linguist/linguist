Gui, -MinimizeBox
Gui, Add, Edit, w300 r5 vText
Gui, Add, Button, x105 w100 Default, Check Pangram
Gui, Show,, Pangram Checker
Return

GuiClose:
    ExitApp
Return

ButtonCheckPangram:
    Gui, Submit, NoHide
    Loop, 26
        If Not InStr(Text, Char := Chr(64 + A_Index)) {
            MsgBox,, Pangram, Character %Char% is missing!
            Return
        }
    MsgBox,, Pangram, OK`, this is a Pangram!
Return
