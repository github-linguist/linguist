OpenConsole()

If ReadFile(0, OpenFileRequester("Text processing/3","mlijobs.txt","All files",1))
  While Not Eof(0)
    currline$=ReadString(0)
    If StringField(currline$,2," ")="OUT"
      counter+1
    Else
      counter-1
    EndIf
    If counter>max
      max=counter
      maxtime$=StringField(currline$,4," ")
    ElseIf counter=max
      maxtime$+#CRLF$+StringField(currline$,4," ")
    EndIf
  Wend
  PrintN(Str(max)+" license(s) used at ;"+#CRLF$+maxtime$)
  CloseFile(0)
Else
  PrintN("Failed to open the file.")
EndIf

PrintN(#CRLF$+"Press ENTER to exit"): Input()
CloseConsole()
