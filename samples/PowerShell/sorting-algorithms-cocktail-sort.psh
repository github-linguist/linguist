function CocktailSort ($a) {
    $l = $a.Length
	$m = 0
	if( $l -gt 1 )
	{
		$hasChanged = $true
		:outer while ($hasChanged) {
			$hasChanged = $false
			$l--
			for ($i = $m; $i -lt $l; $i++) {
				if ($a[$i] -gt $a[$i+1]) {
					$a[$i], $a[$i+1] = $a[$i+1], $a[$i]
					$hasChanged = $true
				}
			}
			if(-not $hasChanged) {
				break outer
			}
			$hasChanged = $false
			for ($i = $l; $i -gt $m; $i--) {
				if ($a[$i-1] -gt $a[$i]) {
					$a[$i-1], $a[$i] = $a[$i], $a[$i-1]
					$hasChanged = $true
				}
			}
			$m++
		}
	}
	$a
}

$l = 10; CocktailSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( -( $l - 1 ), $l - 1 ) } )
