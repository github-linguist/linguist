# Author M. McNabb
function Get-HailStoneBelowLimit {
param($UpperLimit)
    begin {
        function Get-HailStone {
        param($n)
           switch($n) {
              1              {$n;return}
              {$n % 2 -eq 0} {$n; return Get-Hailstone ($n = $n / 2)}
              {$n % 2 -ne 0} {$n; return Get-Hailstone ($n = ($n * 3) +1)}
           }
        }
        $Counts = @()
    }

    process {
        for ($i = 1; $i -lt $UpperLimit; $i++) {
            $Object = [pscustomobject]@{
                'Number' = $i
                'Count' = (Get-HailStone $i).count
            }
            $Counts += $Object
        }
    }
    end {$Counts}
}
