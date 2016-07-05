$Web = New-Object -TypeName Net.Webclient
$Words = $web.DownloadString('http://www.puzzlers.org/pub/wordlists/unixdict.txt')

$IE = $EI = $CIE = $CEI = @()

$Clause1 = $Clause2 = $MainClause = $false

foreach ($Word in $Words.split())
{
    switch ($Word)
    {
        {($_ -like '*ie*') -and ($_ -notlike '*cie*')} {$IE += $Word}
        {($_ -like '*ei*') -and ($_ -notlike '*cei*')} {$EI += $Word}
        {$_ -like '*cei*'} {$CEI += $Word}
        {$_ -like '*cie*'} {$CIE += $Word}
    }
}

if ($IE.count -gt $EI.count * 2)
{$Clause1 = $true}
"The plausibility of 'I before E when not preceded by C' is $Clause1"

if ($CIE.count -gt $CEI.count * 2)
{$Clause2 = $true}
"The plausibility of 'II before E when preceded by C' is $Clause2"

if ($Clause1 -and $Clause2)
{$MainClause = $True}
"The plausibility of the phrase 'I before E except after C' is $MainClause"
