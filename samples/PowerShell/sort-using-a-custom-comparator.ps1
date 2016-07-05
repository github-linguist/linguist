$list = "Here", "are", "some", "sample", "strings", "to", "be", "sorted"
$list | Sort-Object {-$_.Length},{$_}
