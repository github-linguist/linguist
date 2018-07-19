$modificationTime = (Get-ChildItem file.txt).LastWriteTime
Set-ItemProperty file.txt LastWriteTime (Get-Date)

$LastReadTime = (Get-ChildItem file.txt).LastAccessTime
Set-ItemProperty file.txt LastAccessTime(Get-Date)

$CreationTime = (Get-ChildItem file.txt).CreationTime
Set-ItemProperty file.txt CreationTime(Get-Date)
