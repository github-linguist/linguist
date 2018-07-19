Function SelectionSort( [Array] $data )
{
	$datal=$data.length-1
	0..( $datal - 1 ) | ForEach-Object {
		$min = $data[ $_ ]
		$mini = $_
		( $_ + 1 )..$datal | ForEach-Object {
			if( $data[ $_ ] -lt $min ) {
				$min = $data[ $_ ]
				$mini = $_
			}
		}
		$temp = $data[ $_ ]
		$data[ $_ ] = $min
		$data[ $mini ] = $temp
	}
	$data
}

$l = 100; SelectionSort( ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } ) )
