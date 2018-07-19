#Persistent
SetWorkingDir, %A_ScriptDir%
FileDelete, output.txt
EnoughForks := 2 ; required forks to begin eating
Fork1 := Fork2 := Fork3 := Fork4 := Fork5 := 1 ; fork supply per philosopher
SetTimer, AristotleWaitForLeftFork
SetTimer, KantWaitForLeftFork
SetTimer, SpinozaWaitForLeftFork
SetTimer, MarxWaitForLeftFork
SetTimer, RussellWaitForLeftFork
Return ;---------------------------------------------------------------

AristotleWaitForLeftFork:
	WaitForFork("Aristotle", "left", Fork1, Fork2, AristotleLeftForkCount, AristotleRightForkCount, AristotleWaitCount, EnoughForks)
Return
AristotleWaitForRightFork:
	WaitForFork("Aristotle", "right", Fork2, Fork1, AristotleRightForkCount, AristotleLeftForkCount, AristotleWaitCount, EnoughForks)
Return
AristotleFinishEating:
	ReturnForks("Aristotle", Fork1, Fork2, AristotleLeftForkCount, AristotleRightForkCount, EnoughForks)
Return

KantWaitForLeftFork:
	WaitForFork("Kant", "left", Fork2, Fork3, KantLeftForkCount, KantRightForkCount, KantWaitCount, EnoughForks)
Return
KantWaitForRightFork:
	WaitForFork("Kant", "right", Fork3, Fork2, KantRightForkCount, KantLeftForkCount, KantWaitCount, EnoughForks)
Return
KantFinishEating:
	ReturnForks("Kant", Fork2, Fork3, KantLeftForkCount, KantRightForkCount, EnoughForks)
Return

SpinozaWaitForLeftFork:
	WaitForFork("Spinoza", "left", Fork3, Fork4, SpinozaLeftForkCount, SpinozaRightForkCount, SpinozaWaitCount, EnoughForks)
Return
SpinozaWaitForRightFork:
	WaitForFork("Spinoza", "right", Fork4, Fork3, SpinozaRightForkCount, SpinozaLeftForkCount, SpinozaWaitCount, EnoughForks)
Return
SpinozaFinishEating:
	ReturnForks("Spinoza", Fork3, Fork4, SpinozaLeftForkCount, SpinozaRightForkCount, EnoughForks)
Return

MarxWaitForLeftFork:
	WaitForFork("Marx", "left", Fork4, Fork5, MarxLeftForkCount, MarxRightForkCount, MarxWaitCount, EnoughForks)
Return
MarxWaitForRightFork:
	WaitForFork("Marx", "right", Fork5, Fork4, MarxRightForkCount, MarxLeftForkCount, MarxWaitCount, EnoughForks)
Return
MarxFinishEating:
	ReturnForks("Marx", Fork4, Fork5, MarxLeftForkCount, MarxRightForkCount, EnoughForks)
Return

RussellWaitForLeftFork:
	WaitForFork("Russell", "left", Fork5, Fork1, RussellLeftForkCount, RussellRightForkCount, RussellWaitCount, EnoughForks)
Return
RussellWaitForRightFork:
	WaitForFork("Russell", "right", Fork1, Fork5, RussellRightForkCount, RussellLeftForkCount, RussellWaitCount, EnoughForks)
Return
RussellFinishEating:
	ReturnForks("Russell", Fork5, Fork1, RussellLeftForkCount, RussellRightForkCount, EnoughForks)
Return

ReturnForks(Philosopher, ByRef ThisFork, ByRef OtherFork, ByRef CurrentThisForkCount, ByRef CurrentOtherForkCount, EnoughForks) {
	OutputDebug, %Philosopher% finishes eating.
	FileAppend, %Philosopher% finishes eating.`n,output.txt
	ThisFork += CurrentThisForkCount ; return this fork
	OtherFork += CurrentOtherForkCount ; return other fork
	CurrentThisForkCount := 0 ; release this fork
	CurrentOtherForkCount := 0 ; release other fork
	OutputDebug, %Philosopher% returns all forks.
	FileAppend, %Philosopher% returns all forks.`n,output.txt
	
	; do something while resting
	
	Random, Rand, 0, 1
	Rand := Rand ? "Left" : "Right"
	SetTimer, %Philosopher%WaitFor%Rand%Fork
}

WaitForFork(Philosopher, This, ByRef ThisFork, ByRef OtherFork, ByRef CurrentThisForkCount, ByRef CurrentOtherForkCount, ByRef CurrentWaitCount, EnoughForks) {
	If This not in Left,Right
		Return Error
	Other := (This="right") ? "left" : "right"
	OutputDebug, %Philosopher% is hungry.
	FileAppend, %Philosopher% is hungry.`n,output.txt
	If (ThisFork) ; if this fork available
	{
		SetTimer, %Philosopher%WaitFor%This%Fork, Off
		CurrentWaitCount := 0
		ThisFork-- ; take this fork
		CurrentThisForkCount++ ; receive this fork
		OutputDebug, %Philosopher% grabs %This% fork.
		FileAppend, %Philosopher% grabs %This% fork.`n,output.txt
		If (CurrentThisForkCount + CurrentOtherForkCount = EnoughForks) ; if philosopher has enough forks
		{
			OutputDebug, %Philosopher% starts eating.
			FileAppend, %Philosopher% starts eating.`n,output.txt
			
			; do something while eating
		
			SetTimer, %Philosopher%FinishEating, -250
		}
		Else If (EnoughForks=2)
		{
			SetTimer, %Philosopher%WaitFor%Other%Fork
		}
		Else
		{
			Random, Rand, 0, 1
			Rand := Rand ? "Left" : "Right"
			SetTimer, %Philosopher%WaitFor%Rand%Fork
		}
	}
	Else If (CurrentOtherForkCount and CurrentWaitCount > 5) ; if we've been holding other fork too long
	{
		SetTimer, %Philosopher%WaitFor%This%Fork, Off
		CurrentWaitCount := 0
		OtherFork++ ; return other fork
		CurrentOtherForkCount-- ; release other fork
		OutputDebug, %Philosopher% drops %Other% fork.
		FileAppend, %Philosopher% drops %Other% fork.`n,output.txt
		Random, Rand, 0, 1
		Rand := Rand ? "Left" : "Right"
		SetTimer, %Philosopher%WaitFor%Rand%Fork
	}
	Else If (CurrentThisForkCount and CurrentWaitCount > 5) ; if we've been holding one of this fork too long
	{
		SetTimer, %Philosopher%WaitFor%This%Fork, Off
		CurrentWaitCount := 0
		ThisFork++ ; return other fork
		CurrentThisForkCount-- ; release other fork
		OutputDebug, %Philosopher% drops %This% fork.
		FileAppend, %Philosopher% drops %This% fork.`n,output.txt
		Random, Rand, 0, 1
		Rand := Rand ? "Left" : "Right"
		SetTimer, %Philosopher%WaitFor%Rand%Fork
	}
	Else
	{
		CurrentWaitCount++
	}
}
