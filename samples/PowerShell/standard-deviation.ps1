function Get-StandardDeviation {
    begin {
        $avg = 0
        $nums = @()
    }
    process {
        $nums += $_
        $avg = ($nums | Measure-Object -Average).Average
        $sum = 0;
        $nums | ForEach-Object { $sum += ($avg - $_) * ($avg - $_) }
        [Math]::Sqrt($sum / $nums.Length)
    }
}
