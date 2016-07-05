function Forward-Difference( [UInt64] $n, [Array] $f )
{
	$flen = $f.length
	if( $flen -gt [Math]::Max( 1, $n ) )
	{
		0..( $flen - $n - 1 ) | ForEach-Object {
			$l=0;
			for( $k = 0; $k -le $n; $k++ )
			{
				$j = 1
				for( $i = 1; $i -le $k; $i++ )
				{
					$j *= ( ( $n - $k + $i ) / $i )
				}
				$l += $j * ( 1 - 2 * ( ( $n - $k ) % 2 ) ) * $f[ $_ + $k ]
			}
			$l
		}
	}
}

Forward-Difference 2 1,2,4,5
