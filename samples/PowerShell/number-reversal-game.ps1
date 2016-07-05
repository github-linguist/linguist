$Array = 1..9 | Get-Random -Count 9
$nTries = 0
While(-join $Array -ne -join @(1..9)){
    $nTries++
    $nReverse = Read-Host -Prompt "[$Array] -- How many digits to reverse? "
    [Array]::Reverse($Array,0,$nReverse)
}
"$Array"
"Your score: $nTries"
