use Math::Combinatorics;

@n = (0 .. 4);
print join("\n", map { join(" ", @{$_}) } combine(3, @n)), "\n";
