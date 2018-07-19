RPS()

Func RPS()
	Local $ai_Played_games[4]
	$ai_Played_games[0] = 3
	For $I = 1 To 3
		$ai_Played_games[$I] = 1
	Next
	$RPS = GUICreate("Rock Paper Scissors", 338, 108, 292, 248)
	$Rock = GUICtrlCreateButton("Rock", 8, 8, 113, 25, 131072)
	$Paper = GUICtrlCreateButton("Paper", 8, 40, 113, 25, 131072)
	$Scissors = GUICtrlCreateButton("Scissors", 8, 72, 113, 25, 131072)
	$Label1 = GUICtrlCreateLabel("W:", 136, 8, 18, 17)
	$Wins = GUICtrlCreateLabel("0", 160, 8, 36, 17)
	$Label3 = GUICtrlCreateLabel("L:", 208, 8, 13, 17)
	$Looses = GUICtrlCreateLabel("0", 224, 8, 36, 17)
	$Label5 = GUICtrlCreateLabel("D:", 272, 8, 15, 17)
	$Deuce = GUICtrlCreateLabel("0", 296, 8, 36, 17)
	$Displaybutton = GUICtrlCreateButton("", 136, 48, 193, 49, 131072)
	GUICtrlSetState($ai_Played_games, 128)
	GUISetState(@SW_SHOW)
	While 1
		$nMsg = GUIGetMsg()
		Switch $nMsg
			Case -3
				Exit
			Case $Rock
				$Ret = _RPS_Eval(1, $ai_Played_games)
				GUICtrlSetData($Displaybutton, $Ret)
				If $Ret = "Deuce" Then
					GUICtrlSetData($Deuce, Guictrlread($Deuce)+1)
				Elseif $Ret = "You Loose" Then
					GUICtrlSetData($Looses, Guictrlread($Looses)+1)
				Elseif $Ret = "You Win" Then
					GUICtrlSetData($Wins, Guictrlread($Wins)+1)
				EndIf
			Case $Paper
				$Ret = _RPS_Eval(2, $ai_Played_games)
				GUICtrlSetData($Displaybutton, $Ret)
				If $Ret = "Deuce" Then
					GUICtrlSetData($Deuce, Guictrlread($Deuce)+1)
				Elseif $Ret = "You Loose" Then
					GUICtrlSetData($Looses, Guictrlread($Looses)+1)
				Elseif $Ret = "You Win" Then
					GUICtrlSetData($Wins, Guictrlread($Wins)+1)
				EndIf
			Case $Scissors
				$Ret = _RPS_Eval(3, $ai_Played_games)
				GUICtrlSetData($Displaybutton, $Ret)
				If $Ret = "Deuce" Then
					GUICtrlSetData($Deuce, Guictrlread($Deuce)+1)
				Elseif $Ret = "You Loose" Then
					GUICtrlSetData($Looses, Guictrlread($Looses)+1)
				Elseif $Ret = "You Win" Then
					GUICtrlSetData($Wins, Guictrlread($Wins)+1)
				EndIf
		EndSwitch
	WEnd

EndFunc   ;==>RPS

Func _RPS_Eval($i_Player_Choose, $ai_Played_games)
	Local $i_choice = 1
	$i_rnd = Random(1, 1000, 1)
	$i_choose_1 = ($ai_Played_games[1] / $ai_Played_games[0] * 1000)
	$i_choose_2 = ($ai_Played_games[2] / $ai_Played_games[0] * 1000)
	$i_choose_3 = ($ai_Played_games[3] / $ai_Played_games[0] * 1000)
	If $i_rnd < $i_choose_1 Then
		$i_choice = 2
	ElseIf $i_rnd < $i_choose_1 + $i_choose_2 And $i_rnd > $i_choose_1 Then
		$i_choice = 3
	ElseIf $i_rnd < $i_choose_1 + $i_choose_2 + $i_choose_3 And $i_rnd > $i_choose_1 + $i_choose_2 Then
		$i_choice = 1
	EndIf
	$ai_Played_games[0] += 1
	If $i_Player_Choose = 1 Then
		$ai_Played_games[1] += 1
		If $i_choice = 1 Then Return "Deuce"
		If $i_choice = 2 Then Return "You Loose"
		If $i_choice = 3 Then Return "You Win"
	ElseIf $i_Player_Choose = 2 Then
		$ai_Played_games[2] += 1
		If $i_choice = 2 Then Return "Deuce"
		If $i_choice = 3 Then Return "You Loose"
		If $i_choice = 1 Then Return "You Win"
	ElseIf $i_Player_Choose = 3 Then
		$ai_Played_games[3] += 1
		If $i_choice = 3 Then Return "Deuce"
		If $i_choice = 1 Then Return "You Loose"
		If $i_choice = 2 Then Return "You Win"
	EndIf
EndFunc   ;==>_RPS_Eval
