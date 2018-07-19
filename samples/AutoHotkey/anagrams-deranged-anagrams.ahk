Time := A_TickCount
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
SetBatchLines   -1
Loop, Read, unixdict.txt
	StrOut .= StrLen(A_LoopReadLine) - 2 . "," . A_LoopReadLine . "`n"
Sort StrOut, N R
Loop, Parse, StrOut, `n, `r
{
	StringSplit, No_Let, A_Loopfield, `,
	if ( old1 = no_let1 )
		string .= old2 "`n"
	if ( old1 != no_let1 )
	{
		string := trim(string old2)
		if ( old2 != "" )
			Loop, Parse, string, `n, `r  ; Specifying `n prior to `r allows both Windows and Unix files to be Parsed.
				line_number := A_Index
		if ( line_number > 1 )
		{
			Loop, Parse, string, `n, `r
			{
				StringSplit, newstr, A_Loopfield, `, ; break the string based on Comma
				Loop, Parse, newstr2
					k .= A_LoopField " "
				Sort k, D%A_Space%
				k := RegExReplace( k, "\s", "" )
				file .= "`r`n" k . "," . newstr1 . "," . newstr2
				k =
			}
			Sort File
			Loop, Parse, File, `n, `r
			{
				if ( A_Loopfield != "" )
				{
					StringSplit, T_C, A_Loopfield, `,
					if ( old = T_C1 )
					{
						Loop, 1
						{
							Loop % T_C2
								if (SubStr(T_C3, A_Index, 1) = SubStr(old3, A_Index, 1))
									break 2
							Time := (A_tickcount - Time)/1000
							MsgBox % T_C3 " " old3 " in " Time . " seconds."
							ExitApp
						}
					}
					old := T_C1, old3 := T_C3
				}
			}
			file =
		}
		string =
	}
	old1 := no_let1, old2 := A_Loopfield
}
