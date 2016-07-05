function shuffle ($a) {
    $c = $a.Clone()  # make copy to avoid clobbering $a
    1..($c.Length - 1) | ForEach-Object {
        $i = Get-Random -Minimum $_ -Maximum $c.Length
        $c[$_-1],$c[$i] = $c[$i],$c[$_-1]
        $c[$_-1]  # return newly-shuffled value
    }
    $c[-1]  # last value
}

function isSorted( [Array] $data )
{
	$sorted = $true
	for( $i = 1; ( $i -lt $data.length ) -and $sorted; $i++ )
	{
		$sorted = $data[ $i - 1 ] -le $data[ $i ]
	}
	$sorted
}

function BogoSort ( [Array] $indata ) {
	$data = $indata.Clone()
	while( -not ( isSorted $data ) ) {
		$data = shuffle $indata
	}
	$data
}

$l = 7; BogoSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
