CLS

Function isNumeric ($x)
{
    $x2 = 0
    $isNum = [System.Int32]::TryParse($x,[ref]$x2)
Return $isNum
}

$NumberArray = @()
While( $NumberArray.Count -lt 4 ){
    $NumberArray += Random -Minimum 1 -Maximum 10
}

Write-Host @"
Welcome to the 24 game!

Here are your numbers: $($NumberArray -join ",").
Use division, multiplication, subtraction and addition to get 24 as a result with these 4 numbers.
"@

Do
{
$Wrong = 0
$EndResult = $null
$TempChar = $null
$TempChar2 = $null
$Count = $null

$AllowableCharacters = $NumberArray + "+-*/()".ToCharArray()
    $Result = Read-Host
        Foreach($Char in $Result.ToCharArray())
        {
            If( $AllowableCharacters -notcontains $Char ){ $Wrong = 1 }
        }

        If($Wrong -eq 1)
        {
            Write-Warning "Wrong input! Please use only the given numbers."
        }
        Foreach($Char in $Result.ToCharArray())
        {
            If((IsNumeric $TempChar) -AND (IsNumeric $Char))
            {
                Write-Warning "Wrong input! Combining two or more numbers together is not allowed!"
            }
            $TempChar = $Char
        }
        Foreach($Char in $Result.ToCharArray())
        {
            If(IsNumeric $Char)
            {
                $Count++
            }
        }
        If($Count -eq 4)
        {
            $EndResult = Invoke-Expression $Result
                If($EndResult -eq 24)
                {
                    Write-Host "`nYou've won the game!"
                }
                Else
                {
                    Write-Host "`n$EndResult is not 24! Too bad."
                }
        }
        Else
        {
            Write-Warning "Wrong input! You did not supply four numbers."
        }
}
While($EndResult -ne 24)
