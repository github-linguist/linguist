$A = 0
$LogG = 0
$InvH = 0

$ii = 1..10
foreach($i in $ii) {
	# Arithmetic mean is computed directly
	$A += $i / $ii.Count
	# Geometric mean is computed using Logarithms
	$LogG += [Math]::Log($i) / $ii.Count
	# Harmonic mean is computed using its inverse
	$InvH += 1 / ($i * $ii.Count)
}

$G = [Math]::Exp($LogG)
$H = 1/$InvH

write-host "Arithmetic mean: A = $A"
write-host "Geometric mean:  G = $G"
write-host "Harmonic mean:   H = $H"

write-host "Is A >= G ? $($A -ge $G)"
write-host "Is G >= H ? $($G -ge $H)"
