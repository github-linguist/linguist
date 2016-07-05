Function Merge-Array( [Object[]] $lhs, [Object[]] $rhs )
{
	$result = @()
	$lhsl = $lhs.length
	$rhsl = $rhs.length
	if( $lhsl -gt 0 )
	{
		if( $rhsl -gt 0 )
		{
			$i = 0
			for( $j = 0; ( $i -lt $lhsl ) -and ( $j -lt $rhsl ); )
			{
				if( $lhs[ $i ] -le $rhs[ $j ] )
				{
					$result += $lhs[ $i ]
					[void] ( $i++ )
				} else {
					$result += $rhs[ $j ]
					[void] ( $j++ )
				}
			}
			if( $i -lt $lhsl )
			{
				$result += $lhs[ $i..( $lhsl - 1 ) ]
			}
			if( $j -lt $rhsl )
			{
				$result += $rhs[ $j..( $rhsl - 1 ) ]
			}
		} else {
			for( $i = 0; $i -lt $lhsl; $i++ )
			{
				if( $rhs -le $lhs[ $i ] )
				{
					$result += $rhs
					break
				}
				$result += $lhs[ $i ]
			}
			if( $i -lt $lhsl )
			{
				$result += $lhs[ $i..( $lhsl - 1 ) ]
			}
		}
	} else {
		if( $rhsl -gt 0 )
		{
			for( $i = 0; $i -lt $rhsl; $i++ )
			{
				if( $lhs -le $rhs[ $i ] )
				{
					$result += $lhs
					break
				}
				$result += $rhs[ $i ]
			}
			if( $i -lt $rhsl )
			{
				$result += $rhs[ $i..( $rhsl - 1 ) ]
			}
		} else {
			if( $lhs -lt $rhs )
			{
				$result += $lhs
				$result += $rhs
			} else {
				$result += $rhs
				$result += $lhs
			}
		}
	}
	$result
}

Function MergeSort( [Object[]] $data )
{
	$datal = $data.length - 1
	if( $datal -gt 0 )
	{
		$middle = [Math]::Floor( $datal / 2 )
		$left = @()
		$left += MergeSort $data[ 0..$middle ]
		$right = @()
		$right += MergeSort $data[ ( $middle + 1 )..$datal ]
		if( $left[ -1 ] -le $right[ 0 ] )
		{
			$result = @()
			$result += $left
			$result += $right
			$result
		} elseif( $right[ -1 ] -le $left[ 0 ] )
		{
			$result = @()
			$result += $right
			$result += $left
			$result
		} else {
			Merge-Array $left $right
		}
	} else {
		$data
	}
}

$l = 100; MergeSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
