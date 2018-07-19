$irnd = Random(1, 10, 1)
$iinput = -1
While $input <> $irnd
	$iinput = InputBox("Choose a number", "Please chosse a Number between 1 and 10")
WEnd
MsgBox(0, "Success", "Well guessed!")
