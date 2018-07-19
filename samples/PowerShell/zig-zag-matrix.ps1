function zigzag( [int] $n ) {
    $zigzag=New-Object 'Object[,]' $n,$n
    $nodd = $n -band 1
    $nm1 = $n - 1
    $i=0;
    $j=0;
    foreach( $k in 0..( $n * $n - 1 ) ) {
        $zigzag[$i,$j] = $k
        $iodd = $i -band 1
        $jodd = $j -band 1
        if( ( $j -eq $nm1 ) -and ( $iodd -ne $nodd ) ) {
            $i++
        } elseif( ( $i -eq $nm1 ) -and ( $jodd -eq $nodd ) ) {
            $j++
        } elseif( ( $i -eq 0 ) -and ( -not $jodd ) ) {
            $j++
        } elseif( ( $j -eq 0 ) -and $iodd ) {
            $i++
        } elseif( $iodd -eq $jodd ) {
            $i--
            $j++
        } else {
            $i++
            $j--
        }
    }
    ,$zigzag
}

function displayZigZag( [int] $n ) {
    $a = zigzag $n
    0..$n | ForEach-Object {
        $b=$_
        $pad=($n*$n-1).ToString().Length
        "$(0..$n | ForEach-Object {
            "{0,$pad}" -f $a[$b,$_]
        } )"
    }
}
