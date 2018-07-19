$rad = [Math]::PI / 4
$deg = 45
'{0,10} {1,10}' -f 'Radians','Degrees'
'{0,10:N6} {1,10:N6}' -f [Math]::Sin($rad), [Math]::Sin($deg * [Math]::PI / 180)
'{0,10:N6} {1,10:N6}' -f [Math]::Cos($rad), [Math]::Cos($deg * [Math]::PI / 180)
'{0,10:N6} {1,10:N6}' -f [Math]::Tan($rad), [Math]::Tan($deg * [Math]::PI / 180)
$temp = [Math]::Asin([Math]::Sin($rad))
'{0,10:N6} {1,10:N6}' -f $temp, ($temp * 180 / [Math]::PI)
$temp = [Math]::Acos([Math]::Cos($rad))
'{0,10:N6} {1,10:N6}' -f $temp, ($temp * 180 / [Math]::PI)
$temp = [Math]::Atan([Math]::Tan($rad))
'{0,10:N6} {1,10:N6}' -f $temp, ($temp * 180 / [Math]::PI)
