F(n) = n < 1 ? one(n) : n - M(F(n - 1))
M(n) = n < 1 ? zero(n) : n - F(M(n - 1))
