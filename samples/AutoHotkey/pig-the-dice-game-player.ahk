#NoEnv
SetBatchLines, -1
#SingleInstance, Force
#Include Pig_the_dice_game_Optimal_Play.ahkl ; comment if you don't want to bother
Play:=10000 ; this is enough to give 2 digits of accuracy in win ratio
Wins0:=Wins1:=0
Player0(TurnSum, SumMe, SumOpp) {
	Return practical(TurnSum, SumMe, SumOpp) ; set first player function name
}
Player1(TurnSum, SumMe, SumOpp) {
	Return optimal(TurnSum, SumMe, SumOpp) ; set second player function name
}

Loop, % Play
{
	;Random, FirstPlayer, 0, 1 ; to remove advantage of going first
	CurrentPlayer := 0 ; set to FirstPlayer to compare same players with different N's
	Sum0:=Sum1:=0
	Loop
	{
		OtherPlayer:=!CurrentPlayer
		If (Sum%CurrentPlayer%+TurnSum < 100
			and Player%CurrentPlayer%(TurnSum, Sum%CurrentPlayer%, Sum%OtherPlayer%))
		{
		; Roll
			Random, LastRoll, 1, 6
			If (LastRoll != 1)
			{
				TurnSum += LastRoll
				Continue
			}
			TurnSum := 0
		}
		; Hold
		Sum%CurrentPlayer% += TurnSum
		TurnSum := 0
		If (Sum%CurrentPlayer% >= 100)
		{
			Wins%CurrentPlayer%++
			Break
		}
		CurrentPlayer := !CurrentPlayer
	}
}
Msgbox % "Player 0 won " Round(Wins0/Play*100,0) "%`nPlayer 1 won " Round(Wins1/Play*100,0) "%"

; Random; 1/N is ~ probablity of holding
Random(TurnSum, SumMe, SumOpp, N=9) {
	Random, Roll, 0, N ; increase this last number to increase probability of rolling
	Return Roll
}
; Always roll
Always(TurnSum, SumMe, SumOpp) {
	Return 1
}
; Roll N times; N=6 beats all other RollNx players
RollNx(TurnSum, SumMe, SumOpp, N=6) {
	Static Roll=0
	Return Roll := TurnSum = 0 ? 1 : mod(Roll+1,N+1)
}
; Roll if TurnSum < N; N=19 beats all other RollToN players
RollToN(TurnSum, SumMe, SumOpp, N=19) {
	Return Roll := TurnSum < N
}
; Roll if SumOpp > N or SumMe > N or TurnSum < 21 + (SumOpp - SumMe) / 8
Practical(TurnSum, SumMe, SumOpp, N=72) {
	Return Roll := SumOpp > N or SumMe > N or TurnSum < 21+(SumOpp-SumMe)/8
}
; Optimal play per http://cs.gettysburg.edu/~tneller/nsf/pig/pig.pdf
; Requires additional file from [[Pig the dice game/Player/AutoHotkey]]
Optimal(TurnSum, SumMe, SumOpp) {
	Global Optimal
	Roll := Optimal[SumMe,TurnSum,SumOpp+1]
	Return Roll = "" ? 1 : Roll
}
