UrlDownloadToFile
  , http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml
  , tasks.xml
FileRead, tasks, tasks.xml
pos = 0
quote = "  ; "
regtitle := "<cm.*?title=" . quote . "(.*?)" . quote
While, pos := RegExMatch(tasks, regtitle, title, pos + 1)
{
  UrlDownloadToFile
    , % "http://www.rosettacode.org/w/index.php?title=" . title1 . "&action=raw"
    , task.xml
  FileRead, task, task.xml
  RegExReplace(task, "\{\{header\|", "", count)
  current :=  title1 . ": " . count . " examples.`n"
  output .= current
  TrayTip, current, % current
}
MsgBox % output
Return
