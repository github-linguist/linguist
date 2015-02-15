$file = Get-Content c:\file.txt
if ($file.count -lt 7)
{Write-Warning "The file is too short!"}
else
{
    $file | Where Readcount -eq 7 | set-variable -name Line7
}
