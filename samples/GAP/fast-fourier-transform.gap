# Here an implementation with no optimization (O(n^2)).
# In GAP, E(n) = exp(2*i*pi/n), a primitive root of the unity.

Fourier := function(a)
	local n, z;
	n := Size(a);
	z := E(n);
	return List([0 .. n - 1], k -> Sum([0 .. n - 1], j -> a[j + 1]*z^(-k*j)));
end;

InverseFourier := function(a)
	local n, z;
	n := Size(a);
	z := E(n);
	return List([0 .. n - 1], k -> Sum([0 .. n - 1], j -> a[j + 1]*z^(k*j)))/n;
end;

Fourier([1, 1, 1, 1, 0, 0, 0, 0]);
# [ 4, 1-E(8)-E(8)^2-E(8)^3, 0, 1-E(8)+E(8)^2-E(8)^3,
#   0, 1+E(8)-E(8)^2+E(8)^3, 0, 1+E(8)+E(8)^2+E(8)^3 ]

InverseFourier(last);
# [ 1, 1, 1, 1, 0, 0, 0, 0 ]
