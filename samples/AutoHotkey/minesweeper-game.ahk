; Minesweeper.ahk - v1.0.6
; (c) Dec 28, 2008 by derRaphael
; Licensed under the Terms of EUPL 1.0
; Modded by Sobriquet and re-licenced as CeCILL v2
; Modded (again) by Sobriquet and re-licenced as GPL v1.3

#NoEnv
#NoTrayIcon
SetBatchLines,-1
#SingleInstance,Off
/*
[InGameSettings]
*/
Level	=Beginner
Width	=9
Height	=9
MineMax	=10
Marks	=1
Color	=1
Sound	=0
BestTimeBeginner	=999 seconds	Anonymous
BestTimeIntermediate=999 seconds	Anonymous
BestTimeExpert		=999 seconds	Anonymous
; above settings are accessed as variables AND modified by IniWrite - be careful
BlockSize	=16
Title		=Minesweeper
MineCount	=0
mColor		=Blue,Green,Red,Purple,Navy,Olive,Maroon,Teal
GameOver	=0
TimePassed	=0

; Add mines randomly
While (MineCount<MineMax)									; loop as long as neccessary
{
	Random,x,1,%Width%										; get random horizontal position
	Random,y,1,%Height%										; get random vertical position
	If (T_x%x%y%y%!="M")									; only if not already a mine
		T_x%x%y%y%:="M"									; assign as mine
		,MineCount++										; keep count
}

Gui +OwnDialogs
;Menu,Tray,Icon,C:\WINDOWS\system32\winmine.exe,1
Menu,GameMenu,Add,&New	F2,NewGame
Menu,GameMenu,Add
Menu,GameMenu,Add,&Beginner,LevelMenu
Menu,GameMenu,Add,&Intermediate,LevelMenu
Menu,GameMenu,Add,&Expert,LevelMenu
Menu,GameMenu,Add,&Custom...,CustomMenu
Menu,GameMenu,Add
Menu,GameMenu,Add,&Marks (?),ToggleMenu
Menu,GameMenu,Add,Co&lor,ToggleMenu
Menu,GameMenu,Add,&Sound,ToggleMenu
Menu,GameMenu,Add
Menu,GameMenu,Add,Best &Times...,BestTimesMenu
Menu,GameMenu,Add
Menu,GameMenu,Add,E&xit,GuiClose
Menu,GameMenu,Check,% "&" level . (level="Custom" ? "..." : "")
If (Marks)
	Menu,GameMenu,Check,&Marks (?)
If (Color)
	Menu,GameMenu,Check,Co&lor
If (Sound)
	Menu,GameMenu,Check,&Sound

Menu,HelpMenu,Add,&Contents	F1,HelpMenu
Menu,HelpMenu,Add,&Search for Help on...,HelpMenu
Menu,HelpMenu,Add,Using &Help,HelpMenu
Menu,HelpMenu,Add
Menu,HelpMenu,Add,&About Minesweeper...,AboutMenu

Menu,MainMenu,Add,&Game,:GameMenu
Menu,MainMenu,Add,&Help,:HelpMenu
Gui,Menu,MainMenu

Gui,Font,s22,WingDings
Gui,Add,Button,% "h31 w31 y13 gNewGame vNewGame x" Width*BlockSize/2-4,K	; J=Smiley / K=Line / L=Frowny / m=Circle
Gui,Font,s16 Bold,Arial
Gui,Add,Text,% "x13 y14 w45 Border Center vMineCount c" (color ? "Red" : "White"),% SubStr("00" MineCount,-2)	; Remaining mine count
Gui,Add,Text,% "x" Width*BlockSize-34 " y14 w45 Border Center vTimer c" (color ? "Red" : "White"),000			; Timer

; Buttons
Gui,Font,s11,WingDings
Loop,% Height													; iterate vertically
{
	y:=A_Index												; remember line
	Loop,% Width												; set covers
	{
		x:=A_Index
		Gui,Add,Button,% "vB_x" x "y" y	" w" BlockSize 			; mark variable / width
						. " h" BlockSize " " (x = 1				; height / if 1st block
							? "x" 12 " y" y*BlockSize+39		; fix vertical offset
							: "yp x+0")							; otherwise stay inline /w prev. box
	}
}

; Show Gui
Gui,Show,% "w" ((Width>9 ? Width : 9))*BlockSize+24 " h" Height*BlockSize+68,%Title%
OnMessage(0x53,"WM_HELP")										; invoke Help handling for tooltips
Return ;--------------------------------------------------------; End of auto-execute section
#IfWinActive Minesweeper ahk_class AutoHotkeyGUI 				; variable %title% not supported

StartGame(x,y) {
	Global
	If (TimePassed)
		Return
	If (T_x%x%y%y%="M")											; we'll save you this time
	{
		T_x%x%y%y%:=""										; remove this mine
		Loop,% Height
		{
			y:=A_Index
			Loop,% Width
			{
				x:=A_Index
				If ( T_x%x%y%y%!="M")							; add it to first available non-mine square
					T_x%x%y%y%:="M"
					,break:=1
			IfEqual,break,1,Break
			}
		IfEqual,break,1,Break
		}
	}
	Loop,% Height												; iterate over height
	{
		y:=A_Index
		Loop,% Width											; iterate over width
		{
			x:=A_Index
			Gui,Font
			If (T_x%x%y%y%="M") {								; check for mine
				Gui,Font,s11,WingDings
				hColor:="Black"								; set color for text
			} Else {
				Loop,3 {										; loop over neighbors: three columns vertically
					ty:=y-2+A_Index							; calculate top offset
					Loop,3 {									; and three horizontally
						tx:=x-2+A_Index						; calculate left offset
						If (T_x%tx%y%ty%="M")					; no mine and inbound
							T_x%x%y%y%++						; update hint txt
					}
				}
				Gui,Font,s11 Bold,Courier New Bold
				If (Color)
					Loop,Parse,mColor,`,						; find color
						If (T_x%x%y%y% = A_Index) {				; hinttxt to color
							hColor:=A_LoopField				; set color for text
							Break
						}
				Else hColor:="Black"
			}
			Gui,Add,Text,% "c" hColor " Border +Center vT_x" x "y" y	; set color / variable
							. " w" BlockSize " h" BlockSize " "	(x = 1	; width / height / if 1st block
								? "x" 12 " y" y*BlockSize+39			; fix vertical position
								: "yp x+0")								; otherwise align to previous box
							,% T_x%x%y%y%								; M is WingDings for mine
			GuiControl,Hide,T_x%x%y%y%	
		}
	}
	GuiControl,,Timer,% "00" . TimePassed:=1
	SetTimer,UpdateTimer,1000										; start timer
	If (Sound)
		SoundPlay,C:\WINDOWS\media\chord.wav
}

GuiSize:
	If (TimePassed)
		SetTimer,UpdateTimer,On 									; for cheat below
Return

UpdateTimer:														; timer
	GuiControl,,Timer,% (++TimePassed < 999) ? SubStr("00" TimePassed,-2) : 999
	If (Sound)
		SoundPlay,C:\WINDOWS\media\chord.wav
Return

Check(x,y){
	Global
	If (GameOver || B_x%x%y%y% != ""						; the game is over / prevent click on flagged squares and already-opened squares
		|| x<1 || x>Width || y<1 || y>Height)				; do not check neighbor on illegal squares
		Return
	If (T_x%x%y%y%="") {									; empty field?
		CheckNeighbor(x,y)									; uncover it and any neighbours
	} Else If (T_x%x%y%y% != "M") {							; no Mine, ok?
		GuiControl,Hide,B_x%x%y%y%							; hide covering button
		GuiControl,Show,T_x%x%y%y%
		B_x%x%y%y%:=1										; remember we been here
		UpdateChecks()
	} Else {												; ewww ... it's a mine!
		SetTimer,UpdateTimer,Off							; kill timer
		GuiControl,,T_x%x%y%y%,N							; set to Jolly Roger=N to mark death location
		GuiControl,,NewGame,L								; set Smiley Btn to Frown=L
		RevealAll()
		If (Sound)											; do we sound?
			If (FileExist("C:\Program Files\Microsoft Office\Office12\MEDIA\Explode.wav"))
				SoundPlay,C:\Program Files\Microsoft Office\Office12\MEDIA\Explode.wav
			Else
				SoundPlay,C:\WINDOWS\Media\notify.wav
	}
}

CheckNeighbor(x,y) {								; This function checks neighbours of the clicked
	Global											; field and uncovers empty fields plus adjacent hint fields
	If (GameOver || B_x%x%y%y%!="")
		Return
	GuiControl,Hide,B_x%x%y%y%						; uncover it
	GuiControl,Show,T_x%x%y%y%
	B_x%x%y%y%:=1									; remember it
	UpdateChecks()
	If (T_x%x%y%y%!="")								; is neighbour an nonchecked 0 value field?
		Return
	If (y-1>=1)										; check upper neighbour
		CheckNeighbor(x,y-1)
	If (y+1<=Height)								; check lower neighbour
		CheckNeighbor(x,y+1)
	If (x-1>=1)										; check left neighbour
		CheckNeighbor(x-1,y)
	If (x+1<=Width)									; check right neighbour
		CheckNeighbor(x+1,y)
	If (x-1>=1 && y-1>=1)							; check corner neighbour
		CheckNeighbor(x-1,y-1)
	If (x-1>=1 && y+1<=Height)						; check corner neighbour
		CheckNeighbor(x-1,y+1)
	If (x+1<=Width && y-1>=1)						; check corner neighbour
		CheckNeighbor(x+1,y-1)
	If (x+1<=Width && y+1<=Height)					; check corner neighbour
		CheckNeighbor(x+1,y+1)
}

UpdateChecks() {
	Global
	MineCount:=MineMax, Die1:=Die2:=0
	Loop,% Height
	{
		y:=A_Index
		Loop,% Width
		{
			x:=A_Index
			If (B_x%x%y%y%="O")
				MineCount--
			If (T_x%x%y%y%="M" && B_x%x%y%y%!="O") || (T_x%x%y%y%!="M" && B_x%x%y%y%="O")
				Die1++
			If (B_x%x%y%y%="" && T_x%x%y%y%!="M")
				Die2++
		}
	}
	GuiControl,,MineCount,% SubStr("00" MineCount,-2)
	If (Die1 && Die2)
		Return	; only get past here if flags+untouched squares match mines perfectly
	SetTimer,UpdateTimer,Off							; game won - kill timer
	GuiControl,,NewGame,J								; set Smiley Btn to Smile=J
	RevealAll()
	If (Sound)											; play sound
		SoundPlay,C:\WINDOWS\Media\tada.wav
	If (level!="Custom" && TimePassed < SubStr(BestTime%level%,1,InStr(BestTime%level%," ")-1) )
	{
		InputBox,name,Congratulations!,You have the fastest time`nfor level %level%.`nPlease enter your name.`n,,,,,,,,%A_UserName%
		If (name && !ErrorLevel)
		{
			BestTime%level%:=%TimePassed% seconds	%name%
			IniWrite,BestTime%level%,%A_ScriptFullPath%,InGameSettings,BestTime%level%
		}
	}
}

RevealAll(){
	Global
	Loop,% Height										; uncover all
	{
		y:=A_Index
		Loop,% Width
		{
			x:=A_Index
			If(T_x%x%y%y%="M")
			{
				GuiControl,Hide,B_x%x%y%y%
				GuiControl,Show,T_x%x%y%y%
			}
		}
	}
	GameOver:=True									; remember the game's over to block clicks
}

~F2::NewGame()
NewGame(){
NewGame:
	Reload
	ExitApp
}

LevelMenu:
	If (A_ThisMenuItem="&Beginner")
		level:="Beginner", Width:=9, Height:=9, MineMax:=10
	Else If (A_ThisMenuItem="&Intermediate")
		level:="Intermediate", Width:=16, Height:=16, MineMax:=40
	Else If (A_ThisMenuItem="&Expert")
		level:="Expert", Width:=30, Height:=16, MineMax:=99
	IniWrite,%level%,%A_ScriptFullPath%,InGameSettings,level
	IniWrite,%Width%,%A_ScriptFullPath%,InGameSettings,Width
	IniWrite,%Height%,%A_ScriptFullPath%,InGameSettings,Height
	IniWrite,%MineMax%,%A_ScriptFullPath%,InGameSettings,MineMax
	IniWrite,%Marks%,%A_ScriptFullPath%,InGameSettings,Marks
	IniWrite,%Sound%,%A_ScriptFullPath%,InGameSettings,Sound
	IniWrite,%Color%,%A_ScriptFullPath%,InGameSettings,Color
	NewGame()
Return

CustomMenu:												; label for setting window
	Gui,2:+Owner1										; make Gui#2 owned by gameWindow
	Gui,2:Default										; set to default
	Gui,1:+Disabled										; disable gameWindow
	Gui,-MinimizeBox +E0x00000400
	Gui,Add,Text,x15 y36 w38 h16,&Height:
	Gui,Add,Edit,Number x60 y33 w38 h20 vHeight HwndHwndHeight,%Height%	; use inGame settings
	Gui,Add,Text,x15 y60 w38 h16,&Width:
	Gui,Add,Edit,Number x60 y57 w38 h20 vWidth HwndHwndWidth,%Width%
	Gui,Add,Text,x15 y85 w38 h16,&Mines:
	Gui,Add,Edit,Number x60 y81 w38 h20 vMineMax HwndHwndMines,%MineMax%
	Gui,Add,Button,x120 y33 w60 h26 Default HwndHwndOK,OK
	Gui,Add,Button,x120 y75 w60 h26 g2GuiClose HwndHwndCancel,Cancel		; jump directly to 2GuiClose label
	Gui,Show,w195 h138,Custom Field
Return

2ButtonOK:													; label for OK button in settings window
	Gui,2:Submit,NoHide
	level:="Custom"
	MineMax:=MineMax<10 ? 10 : MineMax>((Width-1)*(Height-1)) ? ((Width-1)*(Height-1)) : MineMax
	Width:=Width<9 ? 9 : Width>30 ? 30 : Width
	Height:=Height<9 ? 9 : Height>24 ? 24 : Height
	GoSub,LevelMenu
Return

2GuiClose:
2GuiEscape:
	Gui,1:-Disabled								; reset GUI status and destroy setting window
	Gui,1:Default
	Gui,2:Destroy
Return

ToggleMenu:
	Toggle:=RegExReplace(RegExReplace(A_ThisMenuItem,"\s.*"),"&")
	temp:=%Toggle%
	%Toggle%:=!%Toggle%
	Menu,GameMenu,ToggleCheck,%A_ThisMenuItem%
	IniWrite,% %Toggle%,%A_ScriptFullPath%,InGameSettings,%Toggle%
Return

BestTimesMenu:
	Gui,3:+Owner1
	Gui,3:Default
	Gui,1:+Disabled
	Gui,-MinimizeBox +E0x00000400
	Gui,Add,Text,x15 y22 w250 vBestTimeBeginner HwndHwndBTB,Beginner:	%BestTimeBeginner%
	Gui,Add,Text,x15 y38 w250 vBestTimeIntermediate HwndHwndBTI,Intermediate:	%BestTimeIntermediate%
	Gui,Add,Text,x15 y54 w250 vBestTimeExpert HwndHwndBTE,Expert:		%BestTimeExpert%
	Gui,Add,Button,x38 y89 w75 h20 HwndHwndRS,&Reset Scores
	Gui,Add,Button,x173 y89 w45 h20 Default HwndHwndOK,OK
	Gui,Show,w255 h122,Fastest Mine Sweepers
Return

3ButtonResetScores:
	BestTimeBeginner	=999 seconds	Anonymous
	BestTimeIntermediate=999 seconds	Anonymous
	BestTimeExpert		=999 seconds	Anonymous
	GuiControl,,BestTimeBeginner,Beginner:	%BestTimeBeginner%`nIntermediate:	%BestTimeIntermediate%`nExpert:		%BestTimeExpert%
	GuiControl,,BestTimeIntermediate,Intermediate:	%BestTimeIntermediate%`nExpert:		%BestTimeExpert%
	GuiControl,,BestTimeExpert,Expert:		%BestTimeExpert%
	IniWrite,%BestTimeBeginner%,%A_ScriptFullPath%,InGameSettings,BestTimeBeginner
	IniWrite,%BestTimeIntermediate%,%A_ScriptFullPath%,InGameSettings,BestTimeIntermediate
	IniWrite,%BestTimeExpert%,%A_ScriptFullPath%,InGameSettings,BestTimeExpert
3GuiEscape:	; fall through:
3GuiClose:
3ButtonOK:
	Gui,1:-Disabled									; reset GUI status and destroy setting window
	Gui,1:Default
	Gui,3:Destroy
Return

^q::
GuiClose:
	ExitApp

HelpMenu:
	;Run C:\WINDOWS\Help\winmine.chm
Return

AboutMenu:
	Msgbox,64,About Minesweeper,AutoHotkey (r) Minesweeper`nVersion 1.0.6`nCopyright (c) 2014`nby derRaphael and Sobriquet
Return

~LButton::
~!LButton::
~^LButton::
~#LButton::
	Tooltip
	If (GetKeyState("RButton","P"))
		Return
	If A_OSVersion not in WIN_2003,WIN_XP,WIN_2000,WIN_NT4,WIN_95,WIN_98,WIN_ME
		If(A_PriorHotkey="~*LButton Up" && A_TimeSincePriorHotkey<100)
			GoTo,*MButton Up	; invoke MButton handling for autofill on left double-click in vista+
	If (GameOver)
		Return
	MouseGetPos,,y
	If (y>47)
		GuiControl,,NewGame,m
Return

~*LButton Up::
	If (GetKeyState("RButton","P"))
		GoTo,*MButton Up
	If (GameOver)
		Return
	GuiControl,,NewGame,K
	control:=GetControlFromClassNN(), NumX:=NumY:=0
	RegExMatch(control,"Si)T_x(?P<X>\d+)y(?P<Y>\d+)",Num)		; get Position
	StartGame(NumX,NumY)										; start game if neccessary
	Check(NumX,NumY)
Return

+LButton::
*MButton::
	Tooltip
	If (GameOver)
		Return
	MouseGetPos,,y
	If (y>47)
		GuiControl,,NewGame,m
Return

+LButton Up::
*MButton Up::
	If (GameOver)
		Return
	GuiControl,,NewGame,K
	StartGame(NumX,NumY)											; start game if neccessary
	If (GetKeyState("Esc","P"))
	{
		SetTimer,UpdateTimer,Off
		Return
	}
	control:=GetControlFromClassNN(), NumX:=NumY:=0
	RegExMatch(control,"Si)T_x(?P<X>\d+)y(?P<y>\d+)",Num)
	If ( !NumX || !NumY || B_x%NumX%y%NumY%!=1)
		Return
	temp:=0
	Loop,3 {														; loop over neighbors: three columns vertically
		ty:=NumY-2+A_Index										; calculate top offset
		Loop,3 {													; and three horizontally
			tx:=NumX-2+A_Index									; calculate left offset
			If (B_x%tx%y%ty% = "O")									; count number of marked mines around position
				temp++
		}
	}
	If (temp=T_x%NumX%y%NumY%)
		Loop,3 {													; loop over neighbors: three columns vertically
			ty:=NumY-2+A_Index									; calculate top offset
			Loop,3 {												; and three horizontally
				tx:=NumX-2+A_Index								; calculate left offset
				Check(tx,ty)										; simulate user clicking on each surrounding square
			}
		}
Return

~*RButton::
	If (GameOver || GetKeyState("LButton","P"))
		Return
	control:=GetControlFromClassNN(), NumX:=NumY:=0
	RegExMatch(control,"Si)T_x(?P<X>\d+)y(?P<y>\d+)",Num)			; grab 'em
	If ( !NumX || !NumY || B_x%NumX%y%NumY%=1)
		Return
	StartGame(NumX,NumY)											; start counter if neccessary
	B_x%NumX%y%NumY%:=(B_x%NumX%y%NumY%="" ? "O" : (B_x%NumX%y%NumY%="O" && Marks=1 ? "I" : ""))
	GuiControl,,B_x%NumX%y%NumY%,% B_x%NumX%y%NumY%
	UpdateChecks()
Return

~*RButton Up::
	If (GetKeyState("LButton","P"))
		GoTo,*MButton Up
Return

GetControlFromClassNN(){
	Global width
	MouseGetPos,,,,control
	If (SubStr(control,1,6)="Button")
		NumX:=mod(SubStr(control,7)-2,width)+1,NumY:=(SubStr(control,7)-2)//width+1
	Else If (SubStr(control,1,6)="Static")
		NumX:=mod(SubStr(control,7)-3,width)+1,NumY:=(SubStr(control,7)-3)//width+1
	Return "T_x" NumX "y" NumY
}

WM_HELP(_,_lphi)
{
	Global
	hItemHandle:=NumGet(_lphi+0,12)
	If (hItemHandle=HwndBTB || hItemHandle=HwndBTI || hItemHandle=HwndBTE)
		ToolTip Displays a player's best game time`, and the player's name`, for`neach level of play: Beginner`, Intermediate`, and Expert.
	Else If (hItemHandle=HwndRS)
		ToolTip Click to clear the current high scores.
	Else If (hItemHandle=HwndOK)
		ToolTip Closes the dialog box and saves any changes you`nhave made.
	Else If (hItemHandle=HwndCancel)
		ToolTip Closes the dialog box without saving any changes you have`nmade.
	Else If (hItemHandle=HwndHeight)
		ToolTip Specifies the number of vertical squares on the`nplaying field.
	Else If (hItemHandle=HwndWidth)
		ToolTip Specifies the number of horizontal squares on the`nplaying field.
	Else If (hItemHandle=HwndMines)
		ToolTip Specifies the number of mines to be placed on the`nplaying field.
}

:*?B0:xyzzy:: ; cheat
	KeyWait,Shift,D T1									; 1 sec grace period to press shift
	If (ErrorLevel)
		Return
	While,GetKeyState("Shift","P")
	{
		control:=GetControlFromClassNN()
		If (%control% = "M")
			SplashImage,,B X0 Y0 W1 H1 CW000000			; black pixel
		Else
			SplashImage,,B X0 Y0 W1 H1 CWFFFFFF			; white pixel
		Sleep 100
	}
	SplashImage,Off
Return

/*
 Version History
=================
Dec 29, 2008
1.0.0	Initial Release
1.0.1	BugFix
		- Game Restart Issues mentioned by Frankie
		- Guess count fixup I
		- Startbehaviour of game (time didnt start when 1st click was RMB Guess)
Dec 30, 2008
1.0.2	BugFix
		- Field Size Control vs Max MineCount / mentioned by °digit° / IsNull
1.0.3	BugFix
		- Guess count fixup II mentioned by °digit°
		- Corrected count when 'guessed' field uncovered
Dec 31, 2008
1.0.4	BugFix
		- Fix of Min Field & MineSettings
Mar 7, 2014
1.0.5	AddFeatures
		- Make appearance more like original
		- Make first click always safe
		- Re-license as CeCILL v2
Mar 8, 2014
1.0.6	AddFeatures
		- Add cheat code
		- Add middle-button shortcut
		- Re-license as GPL 1.3
*/
