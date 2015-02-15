GUI, Add, Edit, w150 number vValue, 0 ; Number specifies a numbers-only edit field.
GUI, Add, button,, Increment
GUI, Add, button, xp+70 yp, Decrement ; xp+70 and yp are merely positioning options
GUI, Show, w200 y200, Title	      ; Shows the GUI. Add your own title if you wish
SetTimer, EnableDisable, 100	      ; Sets EnableDisable to run 10 times per second (100ms)
return 				      ; ----------End Auto-Execute Section----------

ButtonIncrement:
    GUI, Submit, NoHide         ; "Set the contents of each variable to the contents of their corresponding controls without hiding the GUI"
    If ( value < 10 )           ; Just in case EnableDisable didn't disable the button it in time.
        Value++                 ; Increment Value
    GUIControl,, Value, %value% ; "Set the text of the control which alters the variable 'value' to the contents of 'value'"
return


ButtonDecrement:
    GUI, Submit, Nohide
    If value > 0
        Value--
    GuiControl,, Value, %value%
return


EnableDisable:
    GUI, Submit, Nohide
    If ( value < 10 )
        GuiControl, enable, Increment
    Else
        GuiControl, disable, Increment

    If ( value > 0)
        GuiControl, enable, Decrement
    Else
        GuiControl, disable, Decrement

    If ( value = 0 )
        GuiControl, enable, Edit1
    Else
        GuiControl, disable, Edit1
return


GuiClose:
    ExitApp
; Ensures the script ends when the GUI is closed.
