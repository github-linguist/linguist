; Author: AlephX, Aug 18 2011
data = %A_scriptdir%\rosettaconfig.txt
comma := ","

Loop, Read, %data%
	{
	if NOT (instr(A_LoopReadLine, "#") == 1 OR A_LoopReadLine == "")

		{
		if instr(A_LoopReadLine, ";") == 1
			{
			parameter := RegExReplace(Substr(A_LoopReadLine,2), "^[ \s]+|[ \s]+$", "")
			%parameter% = "1"		
			}
		else
			{
			parameter := RegExReplace(A_LoopReadLine, "^[ \s]+|[ \s]+$", "")
			
			if instr(parameter, A_Space)
				{
				value := substr(parameter, instr(parameter, A_Space)+1,999)
				parameter := substr(parameter, 1, instr(parameter, A_Space)-1)
				
				if (instr(value, ",") <> 0)
					{
					Loop, Parse, value, %comma% ,%A_Space%
						%parameter%%A_Index% := A_Loopfield					
					}
				else
					%parameter% = %value%
				}
			else
				%parameter% = "0"
			}		
		}
	}
msgbox, FULLNAME %fullname%`nFAVOURITEFRUIT %FAVOURITEFRUIT%`nNEEDSPEELING %NEEDSPEELING%`nSEEDSREMOVED %SEEDSREMOVED%`nOTHERFAMILY %OTHERFAMILY1% + %OTHERFAMILY2%
