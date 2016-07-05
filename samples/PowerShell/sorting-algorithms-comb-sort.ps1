function CombSort ($a) {
    $l = $a.Length
	$gap = 11
	while( $gap -lt $l )
	{
		$gap = [Math]::Floor( $gap*1.3 )
	}
	if( $l -gt 1 )
	{
		$hasChanged = $true
		:outer while ($hasChanged -or ( $gap -gt 1 ) ) {
			$count = 0
			$hasChanged = $false
			if( $gap -gt 1 ) {
				$gap = [Math]::Floor( $gap/1.3 )
			} else {
				$l--
			}
			for ($i = 0; $i -lt ( $l - $gap ); $i++) {
				if ($a[$i] -gt $a[$i+$gap]) {
					$a[$i], $a[$i+$gap] = $a[$i+$gap], $a[$i]
					$hasChanged = $true
					$count++
				}
			}
		}
	}
	$a
}

$l = 100; CombSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( -( $l - 1 ), $l - 1 ) } )
