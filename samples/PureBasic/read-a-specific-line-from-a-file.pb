Structure lineLastRead
  lineRead.i
  line.s
EndStructure

Procedure readNthLine(file, n, *results.lineLastRead)
  *results\lineRead = 0
  While *results\lineRead < n And Not Eof(file)
    *results\line = ReadString(file)
    *results\lineRead + 1
  Wend

  If *results\lineRead = n
    ProcedureReturn 1
  EndIf
EndProcedure

Define filename.s = OpenFileRequester("Choose file to read a line from", "*.*", "All files (*.*)|*.*", 0)
If filename
  Define file = ReadFile(#PB_Any, filename)
  If file
    Define fileReadResults.lineLastRead, lineToRead = 7
    If readNthLine(file, lineToRead, fileReadResults)
      MessageRequester("Results", fileReadResults\line)
    Else
      MessageRequester("Error", "There are less than " + Str(lineToRead) + " lines in file.")
    EndIf
    CloseFile(file)
  Else
    MessageRequester("Error", "Couldn't open file " + filename + ".")
  EndIf
EndIf
