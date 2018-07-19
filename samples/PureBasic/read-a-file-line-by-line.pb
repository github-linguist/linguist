FileName$ = OpenFileRequester("","foo.txt","*.txt",0)

If OpenFile(0, FileName$)
  While Not Eof(0)
    line$ = ReadString(0)
    DoSomethingWithTheLine(Line)
  Wend
  CloseFile(0)
EndIf
