@macro add(x, y) {
	$x + $y
}

@macro mul(x, y) {
	$x * $y
}

@macro double(x) {
	$mul!($x, 2)
}

print($add!(4, $add!(3, 2)))
print($double!(4))