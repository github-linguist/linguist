SetWorkingDir %A_ScriptDir%      ; Change the working directory to the script's location
listFiles := "a.txt|b.txt|c.txt" ; Define a list of files in the current working directory
loop, Parse, listFiles, |
{
	; The above parses the list based on the | character
	fileread, contents, %A_LoopField% ; Read the file
	fileDelete, %A_LoopField%         ; Delete the file
	stringReplace, contents, contents, Goodbye London!, Hello New York!, All ; replace all occurrences
	fileAppend, %contents%, %A_LoopField% ; Re-create the file with new contents
}
