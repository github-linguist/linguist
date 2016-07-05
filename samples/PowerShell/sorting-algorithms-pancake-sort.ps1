Function FlipPancake( [Object[]] $indata, $index = 1 )
{
	$data=$indata.Clone()
	$datal = $data.length - 1
	if( $index -gt 0 )
	{
		if( $datal -gt $index )
		{
			$first = $data[ $index..0 ]
			$last = $data[ ( $index + 1 )..$datal ]
			$data = $first + $last
		} else {
			$data = $data[ $index..0 ]
		}
	}
	$data
}

Function MaxIdx( [Object[]] $data )
{
	$data | ForEach-Object { $max = $data[ 0 ]; $i = 0; $maxi = 0 } { if( $_ -gt $max ) { $max = $_; $maxi = $i }; $i++ } { $maxi }
}

Function PancakeSort( [Object[]] $data, $index = 0 )
{
	"unsorted - $data"
	$datal = $data.length - 1
	if( $datal -gt 0 )
	{
		for( $i = $datal; $i -gt 0; $i-- )
		{
			$data = FlipPancake ( FlipPancake $data ( MaxIdx $data[ 0..$i ] ) ) $i
		}
	}
	"sorted - $data"
}

$l = 100; PancakeSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
