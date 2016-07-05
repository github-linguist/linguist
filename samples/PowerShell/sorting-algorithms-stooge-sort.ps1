Function StoogeSort( [Int32[]] $L )
{
	$i = 0
	$j = $L.length-1
	if( $L[$j] -lt $L[$i] )
	{
		$L[$i] = $L[$i] -bxor $L[$j]
		$L[$j] = $L[$i] -bxor $L[$j]
		$L[$i] = $L[$i] -bxor $L[$j]
	}
	if( $j -gt 1 )
	{
		$t = [int] ( ( $j + 1 ) / 3 )
		$k = $j - $t + 1
		[Array]::Copy( [Int32[]] ( StoogeSort( $L[0..( $j - $t ) ] ) ), $L, $k )
		[Array]::ConstrainedCopy( [Int32[]] ( StoogeSort( $L[$t..$j ] ) ), 0, $L, $t, $k )
		[Array]::Copy( [Int32[]] ( StoogeSort( $L[0..( $j - $t ) ] ) ), $L, $k )
	}
	$L
}

StoogeSort 9, 7, 5, 3, 1, 2, 4, 6, 8
