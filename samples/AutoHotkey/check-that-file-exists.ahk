; FileExist() function examples
ShowFileExist("input.txt")
ShowFileExist("\input.txt")
ShowFolderExist("docs")
ShowFolderExist("\docs")

; IfExist/IfNotExist command examples (from documentation)
IfExist, D:\
  MsgBox, The drive exists.
IfExist, D:\Docs\*.txt
  MsgBox, At least one .txt file exists.
IfNotExist, C:\Temp\FlagFile.txt
  MsgBox, The target file does not exist.

Return

ShowFileExist(file)
{
  If (FileExist(file) && !InStr(FileExist(file), "D"))
    MsgBox, file: %file% exists.
  Else
    MsgBox, file: %file% does NOT exist.
  Return
}

ShowFolderExist(folder)
{
  If InStr(FileExist(folder), "D")
    MsgBox, folder: %folder% exists.
  Else
    MsgBox, folder: %folder% does NOT exist.
  Return
}
