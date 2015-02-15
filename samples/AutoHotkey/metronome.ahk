bpm      = 120 ; Beats per minute
pattern  = 4/4 ;
duration = 100 ; Milliseconds
beats    = 0   ; internal counter

Gui -Caption

StringSplit, p, pattern, /

Start := A_TickCount

Loop
{
	Gui Color, 0xFF0000
	Gui Show, w200 h200 Na
	SoundBeep 750, duration
	beats++
	Sleep 1000 * 60 / bpm - duration
	Loop % p1 -1
	{
		Gui Color, 0x00FF00
		Gui Show, w200 h200 Na
		SoundBeep, , duration
		beats++
		Sleep 1000 * 60 / bpm - duration
	}
}

Esc::
	MsgBox % "Metronome beeped " beats " beats, over " (A_TickCount-Start)/1000 " seconds. "
	ExitApp
