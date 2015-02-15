function Sieve ( [int] $num )
{
    $isprime = @{}
    2..$num | Where-Object {
        $isprime[$_] -eq $null } | ForEach-Object {
        $_
        $isprime[$_] = $true
        $i=$_*$_
        for ( ; $i -le $num; $i += $_ )
        { $isprime[$i] = $false }
    }
}
