function Get-DigitalRoot ($n)
{
    function Get-Digitalsum ($n)
    {
        if ($n -lt 10) {$n}
        else {
            ($n % 10) + (Get-DigitalSum ([math]::Floor($n / 10)))
        }
    }

    $ap = 0
    do {$n = Get-DigitalSum $n; $ap++}
    until ($n -lt 10)
    $DigitalRoot = [pscustomobject]@{
        'Sum' = $n
        'Additive Persistence' = $ap
    }
    $DigitalRoot
}
