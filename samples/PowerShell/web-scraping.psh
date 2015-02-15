$wc = New-Object Net.WebClient
$html = $wc.DownloadString('http://tycho.usno.navy.mil/cgi-bin/timer.pl')
$html -match ', (.*) UTC' | Out-Null
Write-Host $Matches[1]
