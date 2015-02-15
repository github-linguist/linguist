; Author: AlephX, Aug 17 2011
data = %A_scriptdir%\rosettaconfig.txt
outdata = %A_scriptdir%\rosettaconfig.tmp
FileDelete, %outdata%

NUMBEROFBANANAS := 1024
numberofstrawberries := 560
NEEDSPEELING = "0"
FAVOURITEFRUIT := "bananas"
SEEDSREMOVED = "1"
BOOL0 = "0"
BOOL1 = "1"
NUMBER1 := 1
number0 := 0
STRINGA := "string here"

parameters = bool0|bool1|NUMBER1|number0|stringa|NEEDSPEELING|seedsremoved|numberofbananas|numberofstrawberries

Loop, Read, %data%, %outdata%
	{
	if (instr(A_LoopReadLine, "#") == 1 OR A_LoopReadLine == "")
		{
		Line := A_LoopReadLine
		}
	else
		{
		if instr(A_LoopReadLine, ";") == 1
			{
			parameter := RegExReplace(Substr(A_LoopReadLine,2), "^[ \s]+|[ \s]+$", "")

			parametervalue = %parameter%
			value := %parametervalue%
			if value == 0
				Line := A_loopReadLine
			else
				Line := Parameter
			}
		else
			{
			parameter := RegExReplace(A_LoopReadLine, "^[ \s]+|[ \s]+$", "")
			if instr(parameter, A_Space)
				parameter := substr(parameter, 1, instr(parameter, A_Space)-1)
			
			if instr(parameters, parameter) > 0
				{
				parametervalue = %parameter%
				value := %parametervalue%
				
				if (value = chr(34) . "0" . chr(34))
					Line := "; " . Parameter
				else	
					{
					if (value = chr(34) . "1" . chr(34))
						Line := Parameter
					else
						Line = %parametervalue% %value%
					}
				}
			else
				Line := A_LoopReadLine
			}
			
		}
	StringReplace, parameters, parameters, %parametervalue%,,
	StringReplace, parameters, parameters,||,|
	
	FileAppend, %Line%`n
	}

Loop, parse, parameters,|
	{
	if (A_Loopfield <> "")
		{
		StringUpper, parameter, A_LoopField
		parametervalue = %parameter%
		value := %parametervalue%
				
		if (value = chr(34) . "0" . chr(34))
			Line := "; " . parameter
		else	
			{
			if (value = chr(34) . "1" . chr(34))
				Line := parameter
			else
				Line = %parametervalue% %value%
			}
				
		FileAppend, %Line%`n, %outdata%		
		}
	}

FileCopy, %A_scriptdir%\rosettaconfig.tmp, %A_scriptdir%\rosettaconfig.txt, 1
