Function BeadSort ( [Int64[]] $indata )
{
	if( $indata.length -gt 1 )
	{
		$min = $indata[ 0 ]
		$max = $indata[ 0 ]
		for( $i = 1; $i -lt $indata.length; $i++ )
		{
			if( $indata[ $i ] -lt $min )
			{
				$min = $indata[ $i ]
			}
			if( $indata[ $i ] -gt $max ) {
				$max = $indata[ $i ]
			}
		} #Find the min & max
		$poles = New-Object 'UInt64[]' ( $max - $min + 1 )
		$indata | ForEach-Object {
			$min..$_ | ForEach-Object {
				$poles[ $_ - $min ] += 1
			}
		} #Add Beads to the poles, already moved to the bottom
		$min..( $max - 1 ) | ForEach-Object {
			$i = $_ - $min
			if( $poles[ $i ] -gt $poles[ $i + 1 ] )
			{ #No special case needed for min, since there will always be at least 1 = min
				( $poles[ $i ] )..( $poles[ $i + 1 ] + 1 ) | ForEach-Object {
					Write-Output ( $i + $min )
				}
			}
		} #Output the results in pipeline fashion
		1..( $poles[ $max - $min ] ) | ForEach-Object {
			Write-Output $max  #No special case needed for max, since there will always be at least 1 = max
		}
	} else {
		Write-Output $indata
	}
}

$l = 100; BeadSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( -( $l - 1 ), $l - 1 ) } )
