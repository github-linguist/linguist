Function SortThree( [Array] $data )
{
	if( $data[ 0 ] -gt $data[ 1 ] )
	{
		if( $data[ 0 ] -lt $data[ 2 ] )
		{
			$data = $data[ 1, 0, 2 ]
		} elseif ( $data[ 1 ] -lt $data[ 2 ] ){
			$data = $data[ 1, 2, 0 ]
		} else {
			$data = $data[ 2, 1, 0 ]
		}
	} else {
		if( $data[ 0 ] -gt $data[ 2 ] )
		{
			$data = $data[ 2, 0, 1 ]
		} elseif( $data[ 1 ] -gt $data[ 2 ] ) {
			$data = $data[ 0, 2, 1 ]
		}
	}
	$data
}

Function QuickSort( [Array] $data, $rand = ( New-Object Random ) )
{
	$datal = $data.length
	if( $datal -gt 3 )
	{
		[void] $datal--
		$median = ( SortThree $data[ 0, ( $rand.Next( 1, $datal - 1 ) ), -1 ] )[ 1 ]
		$lt = @()
		$eq = @()
		$gt = @()
		$data | ForEach-Object { if( $_ -lt $median ) { $lt += $_ } elseif( $_ -eq $median ) { $eq += $_ } else { $gt += $_ } }
		$lt = ( QuickSort $lt $rand )
		$gt = ( QuickSort $gt $rand )
		$data = @($lt) + $eq + $gt
	} elseif( $datal -eq 3 ) {
		$data = SortThree( $data )
	} elseif( $datal -eq 2 ) {
		if( $data[ 0 ] -gt $data[ 1 ] )
		{
			$data = $data[ 1, 0 ]
		}
	}
	$data
}

QuickSort 5,3,1,2,4
QuickSort 'e','c','a','b','d'
QuickSort 0.5,0.3,0.1,0.2,0.4
$l = 100; QuickSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
