Function ShellSort( [Array] $data )
{
	# http://oeis.org/A108870
	$A108870 = [Int64[]] ( 1, 4, 9, 20, 46, 103, 233, 525, 1182, 2660, 5985, 13467, 30301, 68178, 153401, 345152, 776591, 1747331, 3931496, 8845866, 19903198, 44782196, 100759940, 226709866, 510097200, 1147718700, 2582367076, 5810325920, 13073233321, 29414774973 )
	$datal = $data.length - 1
	$inci = [Array]::BinarySearch( $A108870, [Int64] ( [Math]::Floor( $datal / 2 ) ) )
	if( $inci -lt 0 )
	{
		$inci = ( $inci -bxor -1 ) - 1
	}
	$A108870[ $inci..0 ] | ForEach-Object {
		$inc = $_
		$_..$datal | ForEach-Object {
			$temp = $data[ $_ ]
			$j = $_
			for( ; ( $j -ge $inc ) -and ( $data[ $j - $inc ] -gt $temp ); $j -= $inc )
			{
				$data[ $j ] = $data[ $j - $inc ]
			}
			$data[ $j ] = $temp
		}
	}
	$data
}

$l = 10000; ShellSort( ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } ) )
