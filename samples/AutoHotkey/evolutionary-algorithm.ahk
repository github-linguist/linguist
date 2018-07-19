output := ""
target := "METHINKS IT IS LIKE A WEASEL"
targetLen := StrLen(target)
Loop, 26
	possibilities_%A_Index% := Chr(A_Index+64) ; A-Z
possibilities_27  := " "
C := 100

parent := ""
Loop, %targetLen%
{
	Random, randomNum, 1, 27
  parent .= possibilities_%randomNum%
}

Loop,
{
	If (target = parent)
		Break
	If (Mod(A_Index,10) = 0)
		output .= A_Index ": " parent ", fitness: " fitness(parent, target) "`n"
	bestFit := 0
	Loop, %C%
	  If ((fitness := fitness(spawn := mutate(parent), target)) > bestFit)
		  bestSpawn := spawn , bestFit := fitness
	parent := bestFit > fitness(parent, target) ? bestSpawn : parent
	iter := A_Index
}
output .= parent ", " iter
MsgBox, % output
ExitApp

mutate(parent) {
	local	output, replaceChar, newChar
	output := ""
	Loop, %targetLen%
	{
		Random, replaceChar, 0, 9
		If (replaceChar != 0)
			output .= SubStr(parent, A_Index, 1)
		else
		{
			Random, newChar, 1, 27
			output .= possibilities_%newChar%
		}
	}
	Return output
}

fitness(string, target) {
	totalFit := 0
	Loop, % StrLen(string)
		If (SubStr(string, A_Index, 1) = SubStr(target, A_Index, 1))
			totalFit++
	Return totalFit
}
