DllCall("AllocConsole")
Write("Welcome to Rock-Paper-Scissors`nMake a choice: ")

cR := cP := cS := 0 ; user choice count
userChoice := Read()
Write("My choice: " . cpuChoice := MakeChoice(1, 1, 1))

Loop
{
	Write(DecideWinner(userChoice, cpuChoice) . "`nMake A Choice: ")
	cR += SubStr(userChoice, 1, 1) = "r", cP += InStr(userChoice, "P"), cS += InStr(userChoice, "S")
	userChoice := Read()
	Write("My Choice: " . cpuChoice := MakeChoice(cR, cP, cS))
}

MakeChoice(cR, cP, cS){
	; parameters are number of times user has chosen each item
	total := cR + cP + cS
	
	Random, rand, 0.0, 1.0
	if (rand >= 0 and rand <= cR / total)
		return "Paper"
	else if (rand > cR / total and rand <= (cR + cP) / total)
		return "Scissors"
	else
		return "Rock"
}

DecideWinner(user, cpu){
	user := SubStr(user, 1, 1), cpu := SubStr(cpu, 1, 1)
	if (user = cpu)
		return "`nTie!"
	else if (user = "r" and cpu = "s") or (user = "p" and cpu = "r") or (user = "s" and cpu = "p")
		return "`nYou Win!"
	else
		return "`nI Win!"
}

Read(){
	FileReadLine, a, CONIN$, 1
	return a
}
Write(txt){
	FileAppend, % txt, CONOUT$
}
