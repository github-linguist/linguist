AutoExecute:
    Gui, -MinimizeBox
    Gui, Add, Edit, w500 r20 vInput, 1
    Gui, Add, Button, x155 w100 Default, &Calculate
    Gui, Add, Button, xp+110 yp wp, E&xit
    Gui, Show,, Look-and-Say sequence
Return


ButtonCalculate:
    Gui, Submit, NoHide
    GuiControl,, Input, % LookAndSay(Input)
Return


GuiClose:
ButtonExit:
    ExitApp
Return


;---------------------------------------------------------------------------
LookAndSay(Input) {
;---------------------------------------------------------------------------
    ; credit for this function goes to AutoHotkey forum member Laslo
    ; http://www.autohotkey.com/forum/topic44657-161.html
    ;-----------------------------------------------------------------------
    Loop, Parse, Input          ; look at every digit
        If (A_LoopField = d)    ; I've got another one! (of the same value)
            c += 1                  ; Let's count them ...
        Else {                  ; No, this one is different!
            r .= c d                ; remember what we've got so far
            c := 1                  ; It is the first one in a row
            d := A_LoopField        ; Which one is it?
        }
    Return, r c d
}
