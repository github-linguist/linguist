Loop, {
	Input, Key, L1
	if (Key = "n" || Key = "y")
		break
}
MsgBox, % "The response was """ Key """."
ExitApp
