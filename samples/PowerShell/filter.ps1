$array = -15..37
$array | Where-Object { $_ % 2 -eq 0 }
