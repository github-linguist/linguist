TestString := "Hello World! abcdefg @\;" ; Create a string to be sent with multiple caps and some punctuation
MorseBeep(teststring)                    ; Beeps our string after conversion
return                                   ; End Auto-Execute Section


MorseBeep(passedString)
{
	StringLower, passedString, passedString      ; Convert to lowercase for simpler checking
	loop, parse, passedString                    ; This loop stores each character in A_loopField one by one
	{
		If (A_LoopField = " ")
			morse .= "     "  ; Add a long delay between words (5*e)
		If (A_LoopField = "a")
			morse .=".- "     ; Morse is a local variable
		If (A_LoopField = "b")
			morse .="-... "   ; .= is the simple way of appending to a string
		If (A_LoopField = "c")
			morse .="-.-. "   ; we add a space after every character to pause for e
		If (A_LoopField = "d")
			morse .="-.. "
		If (A_LoopField = "e")
			morse .=". "
		If (A_LoopField = "f")
			morse .="..-. "
		If (A_LoopField = "g")
			morse .="--. "
		If (A_LoopField = "h")
			morse .=".... "
		If (A_LoopField = "i")
			morse .=".. "
		If (A_LoopField = "j")
			morse .=".--- "
		If (A_LoopField = "k")
			morse .="-.- "
		If (A_LoopField = "l")
			morse .=".-.. "
		If (A_LoopField = "m")
			morse .="-- "
		If (A_LoopField = "n")
			morse .="-. "
		If (A_LoopField = "o")
			morse .="--- "
		If (A_LoopField = "p")
			morse .=".--. "
		If (A_LoopField = "q")
			morse .="--.- "
		If (A_LoopField = "r")
			morse .=".-. "
		If (A_LoopField = "s")
			morse .="... "
		If (A_LoopField = "t")
			morse .="- "
		If (A_LoopField = "u")
			morse .="..- "
		If (A_LoopField = "v")
			morse .="...- "
		If (A_LoopField = "w")
			morse .=".-- "
		If (A_LoopField = "x")
			morse .="-..- "
		If (A_LoopField = "y")
			morse .="-.-- "
		If (A_LoopField = "z")
			morse .="--.. "
		If (A_LoopField = "!")
			morse .="---. "
		If (A_LoopField = "\")
			morse .=".-..-. "
		If (A_LoopField = "$")
			morse .="...-..- "
		If (A_LoopField = "'")
			morse .=".----. "
		If (A_LoopField = "(")
			morse .="-.--. "
		If (A_LoopField = ")")
			morse .="-.--.- "
		If (A_LoopField = "+")
			morse .=".-.-. "
		If (A_LoopField = ",")
			morse .="--..-- "
		If (A_LoopField = "-")
			morse .="-....- "
		If (A_LoopField = ".")
			morse .=".-.-.- "
		If (A_LoopField = "/")
			morse .="-..-. "
		If (A_LoopField = "0")
			morse .="----- "
		If (A_LoopField = "1")
			morse .=".---- "
		If (A_LoopField = "2")
			morse .="..--- "
		If (A_LoopField = "3")
			morse .="...-- "
		If (A_LoopField = "4")
			morse .="....- "
		If (A_LoopField = "5")
			morse .="..... "
		If (A_LoopField = "6")
			morse .="-.... "
		If (A_LoopField = "7")
			morse .="--... "
		If (A_LoopField = "8")
			morse .="---.. "
		If (A_LoopField = "9")
			morse .="----. "
		If (A_LoopField = ":")
			morse .="---... "
		If (A_LoopField = ";")
			morse .="-.-.-. "
		If (A_LoopField = "=")
			morse .="-...- "
		If (A_LoopField = "?")
			morse .="..--.. "
		If (A_LoopField = "@")
			morse .=".--.-. "
		If (A_LoopField = "[")
			morse .="-.--. "
		If (A_LoopField = "]")
			morse .="-.--.- "
		If (A_LoopField = "_")
			morse .="..--.- "
	}                                                  ; ---End conversion loop---

	Freq=1280    ; Frequency between 37 and 32767
	e=120        ; element time in milliseconds
		     ; . is one e, - is 3, and a space is a pause of one e
	loop, parse, morse
	{
		if (A_LoopField = ".")
			SoundBeep, Freq, e          ;Format: SoundBeep, frequency, duration

		If (A_LoopField = "-")
			SoundBeep, Freq, 3*e        ; duration can be an expression

		If (A_LoopField = " ")
			Sleep, e                    ; Above, each character is followed by a space, and literal
	}                                           ; spaces are extended. Sleep pauses the script.
} ;                                                         ---End Function Morse---
