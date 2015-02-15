; Create simple windowed application
   Gui, Add, Text, vTextCtl, There have been no clicks yet ; add a Text-Control
   Gui, Add, Button, gButtonClick xm, click me ; add a Button-Control
   Gui, Show, , Simple windowed application ; show the Window
Return ; end of the auto-execute section

ButtonClick: ; the subroutine executed each time the Button-Control is clicked
   count++ ; increment the click-counting var
   GuiControl, , TextCtl, %count% ; update the Text-Control with the click-counting var
Return ; end of the subroutine

GuiClose: ; the subroutine executed when the Window is closed
   ExitApp ; exit this process
Return
