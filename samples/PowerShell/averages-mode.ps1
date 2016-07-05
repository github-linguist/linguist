$data = @(1,1,1,2,3,4,5,5,6,7,7,7)
$groups = $data | group-object | sort-object count -Descending
$groups | ? {$_.Count -eq $groups[0].Count}
