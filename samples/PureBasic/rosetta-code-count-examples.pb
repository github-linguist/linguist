OpenConsole()
URLDownloadToFile_( #Null, "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml", "tasks.xml", 0, #Null)
ReadFile(0, "tasks.xml")
   x1$ =  ReadString(0)
   Repeat
      x2 = FindString(x1$, "title=", x2 + 1)
      If x2
         title$ = Mid(x1$, x2 + 7, 99)
         title$ = Left(title$, FindString(title$, ">", 1) - 4)
         URLDownloadToFile_( #Null, "http://www.rosettacode.org/w/index.php?title=" + title$ + "&action=raw", "task.xml", 0, #Null)
         ReadFile(1, "task.xml")
         While Not Eof(1)
            y1$ =  ReadString(1)
            If FindString(y1$, "=={{header|", 1)
               j + 1
            EndIf
         Wend
         PrintN( title$ +": " + Str(j) + " examples")
         k + j
         j = 0
         CloseFile(1)
      EndIf
  Until x2 = 0
  PrintN("Total: " + Str(k) + " examples")
  Input()
