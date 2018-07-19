Gui, Add, Button, x12 y12 w30 h30 vB1 gButtonHandler,
Gui, Add, Button, x52 y12 w30 h30 vB2 gButtonHandler,
Gui, Add, Button, x92 y12 w30 h30 vB3 gButtonHandler,
Gui, Add, Button, x12 y52 w30 h30 vB4 gButtonHandler,
Gui, Add, Button, x52 y52 w30 h30 vB5 gButtonHandler,
Gui, Add, Button, x92 y52 w30 h30 vB6 gButtonHandler,
Gui, Add, Button, x12 y92 w30 h30 vB7 gButtonHandler,
Gui, Add, Button, x52 y92 w30 h30 vB8 gButtonHandler,
Gui, Add, Button, x92 y92 w30 h30 vB9 gButtonHandler,
; Generated using SmartGUI Creator 4.0
Gui, Show, x127 y87 h150 w141, Tic-Tac-Toe
Winning_Moves := "123,456,789,147,258,369,159,357"
Return

ButtonHandler:
    ; Fired whenever the user clicks on an enabled button
    Go(A_GuiControl,"X")
    GoSub MyMove
Return

MyMove: ; Loops through winning moves. First attempts to win, then to block, then a random move
    Went=0
    Loop, parse, Winning_Moves,`,
    {
        Current_Set := A_LoopField
        X:=O:=0
        Loop, parse, Current_Set
        {
            GuiControlGet, Char,,Button%A_LoopField%
            If ( Char = "O" )
                O++
            If ( Char = "X" )
                X++
        }
        If ( O = 2 and X = 0 ) or ( X = 2 and O = 0 ){
            Finish_Line(Current_Set)
            Went = 1
            Break ; out of the Winning_Moves Loop to ensure the computer goes only once
        }
    }
    If (!Went)
        GoSub RandomMove
Return

Go(Control,chr){
    GuiControl,,%Control%, %chr%
    GuiControl,Disable,%Control%
    GoSub, CheckWin
}

CheckWin:
    Loop, parse, Winning_Moves,`,
    {
        Current_Set := A_LoopField
        X:=O:=0
        Loop, parse, Current_Set
        {
            GuiControlGet, Char,,Button%A_LoopField%
            If ( Char = "O" )
                O++
            If ( Char = "X" )
                X++
        }
        If ( O = 3 ){
            Msgbox O Wins!
            GoSub DisableAll
            Break
        }
        If ( X = 3 ){
            MsgBox X Wins!
            GoSub DisableAll
            Break
        }
    }
return

DisableAll:
    Loop, 9
        GuiControl, Disable, Button%A_Index%
return

Finish_Line(Set){ ;   Finish_Line is called when a line exists with 2 of the same character. It goes in the remaining spot, thereby blocking or winning.
    Loop, parse, set
    {
        GuiControlGet, IsEnabled, Enabled, Button%A_LoopField%
        Control=Button%A_LoopField%
        If IsEnabled
            Go(Control,"O")
    }
}

RandomMove:
    Loop{
        Random, rnd, 1, 9
        GuiControlGet, IsEnabled, Enabled, Button%rnd%
        If IsEnabled
        {
            Control=Button%rnd%
            Go(Control,"O")
            Break
        }
    }
return

GuiClose:
ExitApp
