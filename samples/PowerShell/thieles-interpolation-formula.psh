Function Reciprocal-Difference( [Double[][]] $function )
{
    $rho=@()
    $rho+=0
    $funcl = $function.length
    if( $funcl -gt 0 )
    {
        -2..($funcl-1) | ForEach-Object {
            $i=$_
            #Write-Host "$($i+1) - $($rho[$i+1]) - $($rho[$i+1].GetType())"
            $rho[$i+2] = $( 0..($funcl-$i-1) | Where-Object {$_ -lt $funcl} | ForEach-Object {
                $j=$_
                switch ($i) {
                    {$_ -lt 0 } { 0 }
                    {$_ -eq 0 } { $function[$j][1] }
                    {$_ -gt 0 } { ( $function[$j][0] - $function[$j+$i][0] ) / ( $rho[$i+1][$j] - $rho[$i+1][$j+1] ) + $rho[$i][$j+1] }
                }
            if( $_ -lt $funcl )
            {
                $rho += 0
            }
        })
        }
    }
    $rho
}

Function Thiele-Interpolation ( [Double[][]] $function )
{
    $funcl = $function.length
    $invoke = "{`n`tparam([Double] `$x)`n"
    if($funcl -gt 1)
    {
        $rho = Reciprocal-Difference $function
        ($funcl-1)..0 | ForEach-Object {
            $invoke += "`t"
            $invoke += '$x{0} = {1} - {2}' -f $_, @($rho[$_+2])[0], @($rho[$_])[0]
            if($_ -lt ($funcl-1))
            {
                $invoke += ' + ( $x - {0} ) / $x{1} ' -f $function[$_][0], ($_+1)
            }
            $invoke += "`n"
        }
        $invoke+="`t`$x0`n}"
    } else {
        $invoke += "`t`$x`n}"
    }
    invoke-expression $invoke
}

$sint=@{}; 0..31 | ForEach-Object { $_ * 0.05 } | ForEach-Object { $sint[$_] = [Math]::sin($_) }
$cost=@{}; 0..31 | ForEach-Object { $_ * 0.05 } | ForEach-Object { $cost[$_] = [Math]::cos($_) }
$tant=@{}; 0..31 | ForEach-Object { $_ * 0.05 } | ForEach-Object { $tant[$_] = [Math]::tan($_) }
$asint=New-Object 'Double[][]' 32,2; $sint.GetEnumerator() | Sort-Object Value | ForEach-Object {$i=0}{ $asint[$i][0] = $_.Value; $asint[$i][1] = $_.Name; $i++ }
$acost=New-Object 'Double[][]' 32,2; $cost.GetEnumerator() | Sort-Object Value | ForEach-Object { $i=0 }{ $acost[$i][0] = $_.Value; $acost[$i][1] = $_.Name; $i++ }
$atant=New-Object 'Double[][]' 32,2; $tant.GetEnumerator() | Sort-Object Value | ForEach-Object {$i=0}{ $atant[$i][0] = $_.Value; $atant[$i][1] = $_.Name; $i++ }

$asin = (Thiele-Interpolation $asint)
#uncomment to see the function
#"{$asin}"
6*$asin.InvokeReturnAsIs(.5)
$acos = (Thiele-Interpolation $acost)
#uncomment to see the function
#"{$acos}"
3*$acos.InvokeReturnAsIs(.5)
$atan = (Thiele-Interpolation $atant)
#uncomment to see the function
#"{$atan}"
4*$atan.InvokeReturnAsIs(1)
