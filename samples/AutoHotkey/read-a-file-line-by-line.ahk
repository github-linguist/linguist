; --> Prompt the user to select the file being read

FileSelectFile, File, 1, %A_ScriptDir%, Select the (text) file to read, Documents (*.txt) ; Could of course be set to support other filetypes
If Errorlevel ; If no file selected
	ExitApp

; --> Main loop: Input (File), Output (Text)

Loop
{
FileReadLine, Line, %File%, %A_Index% ; Reads line N (where N is loop iteration)
if Errorlevel ; If line does not exist, break loop
	break
Text .= A_Index ". " Line . "`n" ; Appends the line to the variable "Text", adding line number before & new line after
}

; --> Delivers the output as a text file

FileDelete, Output.txt ; Makes sure output is clear before writing
FileAppend, %Text%, Output.txt ; Writes the result to Output.txt
Run Output.txt ; Shows the created file
