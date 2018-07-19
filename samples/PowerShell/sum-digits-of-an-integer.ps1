function Get-DigitalSum ($n)
{
    if ($n -lt 10) {$n}
    else {
        ($n % 10) + (Get-DigitalSum ([math]::Floor($n / 10)))
    }
}
