$a = [int] (Read-Host a)
$b = [int] (Read-Host b)

if ($a -lt $b) {
    Write-Host $a is less than $b`.
} elseif ($a -eq $b) {
    Write-Host $a is equal to $b`.
} elseif ($a -gt $b) {
    Write-Host $a is greater than $b`.
}
