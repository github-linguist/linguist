; By Uberi, http://www.autohotkey.com/board/topic/58599-turing-machine/
SetBatchLines, -1
OnExit, Exit
SaveFilePath := A_ScriptFullPath ".ini"
; Defaults are for a 2-state_3-symbol turning machine. Format:
; machine state symbol on tape, symbol on tape | tape shift (- is left, + is right, 0 is halt) | machine state
, Rule1 := "A0,1|1|B"
, Rule2 := "A1,2|-1|A"
, Rule3 := "A2,1|-1|A"
, Rule4 := "B0,2|-1|A"
, Rule5 := "B1,2|1|B"
, Rule6 := "B2,0|1|A"
; no error check is run on this input, so be sure states and symbols align with actions
IniRead, UseSaveFile, %SaveFilePath%, Global, UseSaveFile, 1 ; on exit, save state to text file so I can resume on next run
IniRead, MaxIterations, %SaveFilePath%, Global, MaxIterations, 100000 ; set as %A_Space% to run indefinitely
IniRead, Section, %SaveFilePath%, Global, Section, 2-state_3-symbol ; The name of the machine to run. Options defined:
; 2-state_3-symbol
; Simple_incrementer
; Three-state_busy_beaver
; Probable_busy_beaver_Wikipedia

IniRead, States, %SaveFilePath%, %Section%, States, A|B ; valid states
IniRead, InitialState, %SaveFilePath%, %Section%, InitialState, A ; start state
IniRead, TerminalState, %SaveFilePath%, %Section%, TerminalState, C ; end state
IniRead, Symbols, %SaveFilePath%, %Section%, Symbols, 0,1,2 ; valid symbols
IniRead, DefaultCell, %SaveFilePath%, %Section%, DefaultCell, 0 ; the default symbol of any cell not defined on input tape
IniRead, ProgramCode, %SaveFilePath%, %Section%, ProgramCode, 10101|01010 ; start tape
Iniread, RuleCount, %SaveFilePath%, %Section%, RuleCount, 6 ; number of actions to read
Loop, %RuleCount%
{
	IniRead, Temp1, %SaveFilePath%, %Section%, Rule%A_Index%, % Rule%A_Index%
	StringSplit, Temp, Temp1, `,
	Action%Temp1% := Temp2
}

IniRead, Index, %SaveFilePath%, SavedState, Index, 0
IniRead, IterationCount, %SaveFilePath%, SavedState, IterationCount, 0
IniRead, State, %SaveFilePath%, SavedState, State, %InitialState%
If IterationCount > 0
	IniRead, ProgramCode, %SaveFilePath%, SavedState, ProgramCode, %ProgramCode%

IfNotInString, ProgramCode, |
	ProgramCode := "|" ProgramCode
StringSplit, Temp, ProgramCode, |
NegativeCells := Temp1, PositiveCells := Temp2

Loop, Parse, Symbols, |
	Color%A_LoopField% := hex(mod((A_Index+1/(2**((A_Index-1)//7))-1)/7,1)*16777215) ; unlimited number of unique colors
Color%DefaultCell% := "White"

Gui, Color, Black
Gui, +ToolWindow +AlwaysOnTop +LastFound -Caption
WindowID := WinExist()
OnMessage(0x201, "WM_LBUTTONDOWN")
Gui, Font, s6 cWhite, Arial
Loop, 61 ; display 30 cell symbols on each side of current index
{
	Temp1 := ((A_Index - 1) * 15) + 1
	Gui, Add, Progress, x%Temp1% y1 w14 h40 vCell%A_Index% BackgroundWhite
	Gui, Add, Text, x%Temp1% y42 w15 h10 vLabel%A_Index% Center
}
Gui, Add, Text, x2 y54 w26 h10 vState
Gui, Add, Text, x35 y54 w50 h10 vCurrentCell
Gui, Add, Text, x350 y54 w158 h10 vActions
Gui, Add, Text, x844 y54 w33 h10, Iterations:
Gui, Add, Text, x884 y54 w29 h10 vIterations Right
Gui, Font, s4 cWhite Bold, Arial
Gui, Add, Text, x450 y1 w15 h10 Center, V
GuiControl, Move, Cell31, x451 y8 w14 h33
Gui, Show, y20 w916 h64, Wolfram's 2-State 3-Symbol Turing Machine ;'

;MaxIndex := ProgramOffset + StrLen(ProgramCode), MinIndex := ProgramOffset ; not implemented
While, ((MaxIterations = "") || IterationCount <= MaxIterations) ; process until limit is reached, if any
{
	Loop, 61 ; color each cell per its current symbol
	{ ; must run for all displayed cells because they are not directly mapped to shifting tape
		TempIndex := (Index + A_Index) - 31
		GuiControl, , Label%A_Index%, %TempIndex%
		CellColor := CellGet(TempIndex)
		, CellColor := Color%CellColor%
		GuiControl, +Background%CellColor%, Cell%A_Index%
	}
	CurrentCell := CellGet(Index)
	GuiControl, , State, State: %State%
	GuiControl, , CurrentCell, Current Cell: %CurrentCell%
	GuiControl, , Iterations, %IterationCount%
	If (State = TerminalState)
		Break
	
	StringSplit, Temp, Action%State%%CurrentCell%, |
	GuiControl, , Actions, % "Actions: Print " . Temp1 . ", Move " . ((Temp2 = -1) ? "left" : "right") . ", " . ((State <> Temp3) ? "Switch to state " . Temp3 : "Do not switch state")
	
	IterationCount++
	, CellPut(Index,Temp1)
	, Index += Temp2
	, State := Temp3
	;, (Index > MaxIndex) ? MaxIndex := Index : ""
	;, (Index < MinIndex) ? MinIndex := Index : ""
	
	Sleep, 0.1*1000
}
MsgBox, 64, Complete, Completed %IterationCount% iterations of the Turing machine.
Return


; Hotkeys and functions:
~Pause::Pause

GuiEscape:
GuiClose:
	ExitApp

Exit:
	If UseSaveFile
	{
		IniWrite, %Index%, %SaveFilePath%, %Section%, Index
		IniWrite, %IterationCount%, %SaveFilePath%, %Section%, IterationCount
		IniWrite, %State%, %SaveFilePath%, %Section%, State
		IniWrite, %NegativeCells%|%PositiveCells%, %SaveFilePath%, %Section%, ProgramCode
	}
	ExitApp

CellGet(Index)
{
	global NegativeCells, PositiveCells, DefaultCell
	Temp1 := (Index < 0) ? SubStr(NegativeCells,Abs(Index),1) : SubStr(PositiveCells,Index + 1,1)
	Return, (Temp1 = "") ? DefaultCell : Temp1
}

CellPut(Index,Char)
{
	global NegativeCells, PositiveCells, DefaultCell
	static StrGetFunc := "StrGet" ; workaround to hide function from AHK Basic (which does not have or require it)
	CharType := A_IsUnicode ? "UShort" : "UChar"
	, (Index < 0)
		? (Index := 0 - Index
		, Temp1 := Index - StrLen(NegativeCells)
		, (Temp1 > 0)
			? (VarSetCapacity(Pad,64) ; these three functions are quirks in AHK's memory management (not required)
			, VarSetCapacity(Pad,0)
			, VarSetCapacity(Pad,Temp1,Asc(DefaultCell))
			, NegativeCells .= A_IsUnicode ? %StrGetFunc%(&Pad,Temp1,"CP0") : Pad)
			: ""
		, NumPut(Asc(Char),NegativeCells,(Index - 1) << !!A_IsUnicode,CharType)		)
		: (Temp1 := Index - StrLen(PositiveCells) + 1
		, (Temp1 > 0)
			? (VarSetCapacity(Pad,64) ; these three functions are quirks in AHK's memory management (not required)
			, VarSetCapacity(Pad,0)
			, VarSetCapacity(Pad,Temp1,Asc(DefaultCell))
			, PositiveCells .= A_IsUnicode ? %StrGetFunc%(&Pad,Temp1,"CP0") : Pad)
			: ""
		, NumPut(Asc(Char),PositiveCells,Index << !!A_IsUnicode,CharType)		)
}

Hex(p_Integer)
{
	PtrType:=(A_PtrSize=8) ? "Ptr":"UInt"
	l_Format:="`%0" . 6 . "I64X"
	VarSetCapacity(l_Argument,8)
	NumPut(p_Integer,l_Argument,0,"Int64")
	VarSetCapacity(l_Buffer,A_IsUnicode ? 12:6,0)
	DllCall(A_IsUnicode ? "msvcrt\_vsnwprintf":"msvcrt\_vsnprintf"
		,"Str",l_Buffer ;-- Storage location for output
		,"UInt",6 ;-- Maximum number of characters to write
		,"Str",l_Format ;-- Format specification
		,PtrType,&l_Argument) ;-- Argument
	Return l_Buffer
}

WM_LBUTTONDOWN()
{
	If (A_Gui = 1)
	PostMessage, 0xA1, 2
}
