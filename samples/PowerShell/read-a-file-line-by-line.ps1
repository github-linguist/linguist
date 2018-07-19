$reader = [System.IO.File]::OpenText($mancorfile)
try {
	do {
		$line = $reader.ReadLine()
		if ($line -eq $null) { break }
		DoSomethingWithLine($line)
	} while ($TRUE)
} finally {
	$reader.Close()
}
