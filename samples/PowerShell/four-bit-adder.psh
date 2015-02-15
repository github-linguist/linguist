function bxor2 ( [byte] $a, [byte] $b )
{
    $out1 = $a -band ( -bnot $b )
    $out2 = ( -bnot $a ) -band $b
    $out1 -bor $out2
}

function hadder ( [byte] $a, [byte] $b )
{
    @{
        "S"=bxor2 $a $b
        "C"=$a -band $b
    }
}

function fadder ( [byte] $a, [byte] $b, [byte] $cd )
{
    $out1 = hadder $cd $a
    $out2 = hadder $out1["S"] $b
    @{
        "S"=$out2["S"]
        "C"=$out1["C"] -bor $out2["C"]
    }
}

function FourBitAdder ( [byte] $a, [byte] $b )
{
    $a0 = $a -band 1
    $a1 = ($a -band 2)/2
    $a2 = ($a -band 4)/4
    $a3 = ($a -band 8)/8
    $b0 = $b -band 1
    $b1 = ($b -band 2)/2
    $b2 = ($b -band 4)/4
    $b3 = ($b -band 8)/8
    $out1 = fadder $a0 $b0 0
    $out2 = fadder $a1 $b1 $out1["C"]
    $out3 = fadder $a2 $b2 $out2["C"]
    $out4 = fadder $a3 $b3 $out3["C"]
    @{
        "S"="{3}{2}{1}{0}" -f $out1["S"], $out2["S"], $out3["S"], $out4["S"]
        "V"=$out4["C"]
    }
}

FourBitAdder 3 5

FourBitAdder 0xA 5

FourBitAdder 0xC 0xB

[Convert]::ToByte((FourBitAdder 0xC 0xB)["S"],2)
