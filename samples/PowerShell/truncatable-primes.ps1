function IsPrime ( [int] $num )
{
    $isprime = @{}
    2..[math]::sqrt($num) | Where-Object {
        $isprime[$_] -eq $null } | ForEach-Object {
        $_
        $isprime[$_] = $true
        for ( $i=$_*$_ ; $i -le $num; $i += $_ )
        { $isprime[$i] = $false }
    }
    2..$num | Where-Object { $isprime[$_] -eq $null }
}

function Truncatable ( [int] $num )
{
    $declen = [math]::abs($num).ToString().Length
    $primes = @()
    $ltprimes = @{}
    $rtprimes = @{}
    1..$declen | ForEach-Object { $ltprimes[$_]=@{}; $rtprimes[$_]=@{} }
    IsPrime $num | ForEach-Object {
        $lastltprime = 2
        $lastrtprime = 2
    } {
        $curprim = $_
        $curdeclen = $curprim.ToString().Length
        $primes += $curprim
        if( $curdeclen -eq 1 ) {
            $ltprimes[1][$curprim] = $true
            $rtprimes[1][$curprim] = $true
            $lastltprime = $curprim
            $lastrtprime = $curprim
        } else {
            $curmod = $curprim % [math]::pow(10,$curdeclen - 1)
            $curdiv = [math]::floor($curprim / 10)
            if( $ltprimes[$curdeclen - 1][[int]$curmod] ) {
                $ltprimes[$curdeclen][$curprim] = $true
                $lastltprime = $curprim
            }
            if( $rtprimes[$curdeclen - 1][[int]$curdiv] ) {
                $rtprimes[$curdeclen][$curprim] = $true
                $lastrtprime = $curprim
            }
        }
        if( ( $ltprimes[$curdeclen - 2].Keys.count -gt 0 ) -and ( $ltprimes[$curdeclen - 1].Keys.count -gt 0 ) ) { $ltprimes[$curdeclen -2] = @{} }
        if( ( $rtprimes[$curdeclen - 2].Keys.count -gt 0 ) -and ( $rtprimes[$curdeclen - 1].Keys.count -gt 0 ) ) { $rtprimes[$curdeclen -2] = @{} }
    } {
        "Largest Left Truncatable Prime: $lastltprime"
        "Largest Right Truncatable Prime: $lastrtprime"
    }
}
