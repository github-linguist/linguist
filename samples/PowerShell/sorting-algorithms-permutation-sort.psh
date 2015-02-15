Function PermutationSort( [Object[]] $indata, $index = 0, $k = 0 )
{
	$data = $indata.Clone()
	$datal = $data.length - 1
	if( $datal -gt 0 ) {
		for( $j = $index; $j -lt $datal; $j++ )
		{
			$sorted = ( PermutationSort $data ( $index + 1 ) $j )[0]
			if( -not $sorted )
			{
				$temp = $data[ $index ]
				$data[ $index ] = $data[ $j + 1 ]
				$data[ $j + 1 ] = $temp
			}
		}
		if( $index -lt ( $datal - 1 ) )
		{
			PermutationSort $data ( $index + 1 ) $j
		} else {
			$sorted = $true
			for( $i = 0; ( $i -lt $datal ) -and $sorted; $i++ )
			{
				$sorted = ( $data[ $i ] -le $data[ $i + 1 ] )
			}
			$sorted
			$data
		}
	}
}

0..4 | ForEach-Object { $a = $_; 0..4 | Where-Object { -not ( $_ -match "$a" ) } |
	ForEach-Object { $b = $_; 0..4 | Where-Object { -not ( $_ -match "$a|$b" ) } |
		ForEach-Object { $c = $_; 0..4 | Where-Object { -not ( $_ -match "$a|$b|$c" ) } |
			ForEach-Object { $d = $_; 0..4 | Where-Object { -not ( $_ -match "$a|$b|$c|$d" ) } |
				ForEach-Object { $e=$_; "$( PermutationSort ( $a, $b, $c, $d, $e ) )" }
			}
		}
	}
}
$l = 8; PermutationSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
