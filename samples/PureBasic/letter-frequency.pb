Procedure countLetters(Array letterCounts(1), textLine.s)
  ;counts only letters A -> Z, uses index 0 of letterCounts() to keep a total of all counts
  Protected i, lineLength = Len(textLine), letter

  textLine = UCase(textLine)
  For i = 1 To lineLength
    letter = Asc(Mid(textLine, i, 1)) - 'A' + 1
    If letter >= 1 And letter <= 26
      letterCounts(letter) + 1 ;tally individual letter count
      letterCounts(0) + 1      ;increment total letter count
    EndIf
  Next
EndProcedure

If OpenConsole()
  Define filename.s, fileID, i
  filename = OpenFileRequester("Select text file to examine", "*.txt", "Text (*.txt)|*.txt;|All files (*.*)|*.*", 0)
  fileID = 0
  If ReadFile(fileID, filename)
    Dim letterCounts(26) ;A - Z only, index 0 contains the total of all letter counts

    Define textLine.s
    While Not Eof(fileID)
      textLine = ReadString(fileID)
      countLetters(letterCounts(), textLine)
    Wend
    CloseFile(fileID)

    PrintN("File: " + filename + #CRLF$)
    PrintN("Letter  %Freq  Count")
    For i = 1 To 26
      Print("  " + Chr(64 + i) + "     ")
      Print(RSet(StrF(100 * letterCounts(i) / letterCounts(0), 1), 5, " ") + "  ")
      PrintN(Str(letterCounts(i)))
    Next
    PrintN(#CRLF$ + "Total letter count in file: " + Str(letterCounts(0)))
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
