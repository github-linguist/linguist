IfNotExist, mlijobs.txt
  UrlDownloadToFile, http://rosettacode.org/mlijobs.txt, mlijobs.txt

out := 0, max_out := -1, max_times := ""

Loop, Read, mlijobs.txt
{
  If InStr(A_LoopReadLine, "OUT")
    out++
  Else
    out--
  If (out > max_out)
    max_out := out, max_times := ""
  If (out = max_out)
  {
    StringSplit, lineArr, A_LoopReadLine, %A_Space%
    max_times .= lineArr4 . "`n"
  }
}

MsgBox Maximum use is %max_out% at:`n`n%max_times%
